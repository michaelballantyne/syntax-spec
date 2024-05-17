#lang racket

(provide (all-defined-out))

(require "../../../main.rkt"
         racket/stxparam
         syntax/transformer
         (for-syntax racket/list
                     syntax/parse
                     syntax/transformer))

(struct class-info [name->method-index method-table constructor])
;; A ClassInfo is a (class-info (symbol -> natural) (any ... -> Object)) where
;; name->method-index maps a method name to its vector index in the method-table
;; method-table is a vector of methods
;; Represents a class itself

(struct object [fields class])
;; An Object is a (object (vector any) (vector Method) Class) where
;; fields is a vector of field-values
;; class is the class of which this object is an instance
;; Represents an object, which is an instance of a class

;; A Method is a (any any ... -> any)
;; Where the first argument is "this"
;; Represents a method on a class

(define-syntax-parameter this
  (make-expression-transformer
   (syntax-parser
     [_ (raise-syntax-error 'this "used outside of a class" this-syntax)])))

(begin-for-syntax
  (define-syntax-class lambda-id
    (pattern (~or (~literal lambda) (~literal #%plain-lambda)))))

(syntax-spec
  (binding-class method-var #:description "method name")
  (binding-class field-var #:description "field name")

  (nonterminal/exporting class-form
                        #:allow-extension racket-macro
                        (field name:field-var ...)
                        #:binding (export name)
                        ((~literal define-values) (m:method-var) (lambda:lambda-id (arg:id ...) body:racket-expr ...))
                        #:binding (export m)

                        ((~literal define-syntaxes) (x:racket-macro ...) e:expr)
                        #:binding (export-syntaxes x e)

                        ((~literal begin) e:class-form ...)
                        #:binding (re-export e)

                        e:racket-expr)

  (host-interface/expression
    (class e:class-form ...)
    #:binding (scope (import e))
    (define-values (defns fields exprs) (group-class-decls (splice-begins (attribute e))))
    (compile-class-body defns fields exprs)))

(begin-for-syntax
  (define-persistent-symbol-table field-index-table)

  #;((listof syntax?) -> (listof syntax?))
  ;; splices begins (recursively), returns flattened list of exprs.
  (define (splice-begins exprs)
    (syntax-parse exprs
      [() this-syntax]
      [(expr . rest-exprs)
       (syntax-parse #'expr
         #:literals (begin)
         [(begin e ...)
          (splice-begins (append (attribute e) #'rest-exprs))]
         [_ (cons this-syntax (splice-begins #'rest-exprs))])]))

  #;((listof syntax?) -> (values (listof syntax?) (listof syntax?) (listof syntax?)))
  ;; accepts a list of partially expanded class-level definitions and returns them grouped into
  ;; syntax definitions, value definitions, field declarations, and top-level exprs
  (define (group-class-decls exprs)
    (syntax-parse exprs
      #:literals (define-values define-syntaxes field)
      [((~alt (~and defn (define-values . _))
              ;; ignore because they don't end up in the generated code
              (~and stx-defn (define-syntaxes . _))
              (~and field-decl (field . _))
              expr)
        ...)
       (values (attribute defn)
               (attribute field-decl)
               (attribute expr))]))

  #;((listof syntax?) (listof syntax?) (listof syntax?) -> syntax?)
  ;; compile the partially expanded class-level definitions into pure racket code.
  ;; This is the actual class logic.
  (define (compile-class-body defns fields exprs)
    (syntax-parse (list defns fields exprs)
      #:literals (define-values field)
      [(((define-values (method-name:id) (_ (method-arg:id ...) method-body:expr ...)) ...)
        ;; only 1 field definition allowed
        ((~optional (field field-name:id ...) #:defaults ([(field-name 1) null])))
        (expr ...))
       (check-duplicate-method-names (attribute method-name))
       (for ([field-name (attribute field-name)]
             [field-index (in-naturals)])
         (symbol-table-set! field-index-table field-name field-index))
       #'(with-reference-compilers ([method-var method-reference-compiler]
                                    [field-var field-reference-compiler])
           (letrec ([method-table
                     (vector (lambda (this-arg method-arg ...)
                               (syntax-parameterize ([this (make-variable-like-transformer #'this-arg)])
                                 method-body
                                 ...))
                             ...)]
                    [constructor
                     (lambda (field-name ...)
                       (let ([this-val (object (vector field-name ...) cls)])
                         (syntax-parameterize ([this (make-variable-like-transformer #'this-val)])
                           ;; ensure body is non-empty
                           (void)
                           expr
                           ...)
                         this-val))]
                    [method-name->index
                     (make-name->index (list 'method-name ...))]
                    [cls
                     (class-info method-name->index method-table constructor)])
             cls))]))

  (define method-reference-compiler
    (make-variable-like-reference-compiler
     (syntax-parser
       [name:id
        #'(lambda args (send this name . args))])))

  (define field-reference-compiler
    (make-variable-like-reference-compiler
     (syntax-parser
       [name:id
        (let ([idx (symbol-table-ref field-index-table #'name)])
          #`(vector-ref (object-fields this) #,idx))])
     (syntax-parser
       [(_ name:id rhs)
        (let ([idx (symbol-table-ref field-index-table #'name)])
          #`(vector-set! (object-fields this) #,idx rhs))])))

  #;((listof identifier?) -> void?)
  ;; If there are (symbolically) duplicate method names, error
  (define (check-duplicate-method-names names)
    (let ([duplicate (check-duplicates names #:key syntax->datum)])
      (when duplicate
        (raise-syntax-error #f "a method with same name has already been defined" duplicate)))))

#;((listof symbol?) -> (symbol? -> natural?))
;; Create a function that maps method names to their method table indices
(define (make-name->index names)
  (let ([table (for/hasheq ([name names]
                            [idx (in-naturals)])
                 (values name idx))])
    (lambda (name)
      (hash-ref table name (lambda () (error 'send "no such method ~a" name))))))

(define (new cls . fields)
  (apply (class-info-constructor cls) fields))

(define-syntax send
  (syntax-parser
    [(_ obj:expr method-name:id arg:expr ...)
     #'(send-rt obj 'method-name (list arg ...))]
    [(_ obj:expr method-name:id . args)
     #'(send-rt obj 'method-name args)]))

#;(object? symbol? (listof any/c) -> any/c)
(define (send-rt obj method-name args)
  (let* ([cls (object-class obj)]
         [index ((class-info-name->method-index cls) method-name)]
         [method-table (class-info-method-table cls)]
         [method (vector-ref method-table index)])
    (apply method obj args)))
