#lang racket

(provide (all-defined-out))

(require "../../../main.rkt"
         racket/stxparam
         (for-syntax racket/list
                     syntax/parse
                     syntax/transformer))

(struct class-info [methods constructor])
;; A ClassInfo is a (class-info (HashEq Symbol Method) (any ... -> Object)) where
;; methods maps a method name to its implementation
;; constructor creates an instance of the class
;; Represents a class itself

(struct object [fields class])
;; An Object is a (object (VectorOf any) (VectorOf Method) Class) where
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
  (binding-class method-var
                 #:description "method name"
                 #:reference-compiler method-reference-compiler)
  (binding-class field-var
                 #:description "field name"
                 #:reference-compiler field-reference-compiler)

  (nonterminal/exporting class-form
    #:allow-extension racket-macro
    (field name:field-var ...)
    #:binding [(export name) ...]
    ((~literal define-values) (m:method-var)
      (lambda:lambda-id (arg:racket-var ...) body:racket-body ...))
    #:binding [(export m) (scope (bind arg) ... (import body) ...)]

    ((~literal define-syntaxes) (x:racket-macro ...) e:expr)
    #:binding (export-syntaxes x ... e)

    ((~literal begin) e:class-form ...)
    #:binding [(re-export e) ...]

    e:racket-body
    #:binding (re-export e))

  (host-interface/expression
    (class e:class-form ...)
    #:binding (scope (import e) ...)
    (define-values (defns fields constructor-body) (group-class-decls (splice-begins (attribute e))))
    (compile-class-body defns fields constructor-body)))

(begin-for-syntax
  (define field-index-table (local-symbol-table))

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
              (~and stx-defn (define-syntaxes . _))
              (field field-name ...)
              constructor-body)
        ...)
       ;; discard stx-defn because syntax definitions don't end up in the generated code
       (values (attribute defn)
               #'(field-name ... ...)
               (attribute constructor-body))]))

  #;((listof syntax?) (listof syntax?) (listof syntax?) -> syntax?)
  ;; compile the partially expanded class-level definitions into pure racket code.
  ;; This is the actual class logic.
  (define (compile-class-body defns fields constructor-body)
    (syntax-parse (list defns fields constructor-body)
      #:literals (define-values field)
      [(((define-values (method-name:id) (_ (method-arg:id ...) method-body:expr ...)) ...)
        (field-name:id ...)
        (constructor-body ...))
       (define/syntax-parse method-table (compile-methods (attribute method-name) (attribute method-arg) (attribute method-body)))
       (define/syntax-parse constructor-procedure (compile-constructor (attribute field-name) #'cls (attribute constructor-body)))
       #'(letrec ([methods method-table]
                  [constructor constructor-procedure]
                  [cls (class-info methods constructor)])
           cls)]))
  
  #;((listof identifier?) (listof (listof identifier?)) (listof syntax?) -> syntax?)
  (define (compile-methods method-name method-arg method-body)
    (check-duplicate-method-names method-name)
    (syntax-parse (list method-name method-arg method-body)
      [((method-name ...) ((method-arg ...) ...) ((method-body ...) ...))
       #'(make-immutable-hash
          (list
           (cons 'method-name
                 (lambda (this-arg method-arg ...)
                   (syntax-parameterize ([this (make-variable-like-transformer #'this-arg)])
                     method-body
                     ...)))
           ...))]))

  #;((listof identifier?) identifier? (listof syntax?) -> syntax?)
  (define (compile-constructor field-name cls constructor-body)
    (for ([field-name field-name]
          [field-index (in-naturals)])
      (symbol-table-set! field-index-table field-name field-index))
    (syntax-parse (list field-name cls constructor-body)
      [((field-name ...) cls (constructor-body ...))
       #'(lambda (field-name ...)
           (let ([this-val (object (vector field-name ...) cls)])
             (syntax-parameterize ([this (make-variable-like-transformer #'this-val)])
               ;; ensure body is non-empty
               (void)
               constructor-body
               ...)
             this-val))]))

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
         [method (hash-ref (class-info-methods cls) method-name (lambda () (error 'send "unknown method ~a" method-name)))])
    (apply method obj args)))
