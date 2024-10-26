#lang racket/base

(provide #%host-expression
         with-reference-compilers
         (for-syntax add-global-reference-compiler!
                     binding-as-rkt
                     make-suspension

                     mutable-reference-compiler
                     immutable-reference-compiler

                     make-variable-like-reference-compiler))

(require
  syntax/parse
  (for-syntax
   racket/base
   racket/promise
   racket/list
   racket/function
   racket/private/check
   racket/match
   syntax/parse
   "../syntax/env-reps.rkt"
   syntax/id-table
   syntax/transformer
   racket/syntax
   "../ee-lib/main.rkt")
  (for-meta 2
            racket/base
            syntax/parse
            "../syntax/env-reps.rkt"
            "../ee-lib/main.rkt"))

(define-syntax call-3d-syntax
    (syntax-parser
      [(_ proc arg)
       ((syntax-e #'proc) #'arg)]))

(begin-for-syntax
  ; (Syntax | (Identifier -> Syntax)) {Syntax | (Syntax -> Syntax)}
  ; Like make-variable-like-transformer.
  ; If reference-stx is Syntax, replace references with it.
  ; If reference-stx is a procedure, apply it to the reference syntax.
  ; If setter-stx is Syntax, it should be syntax for a procedure accepting
  ; the new value for the variable.
  ; If setter-stx is a procedure, apply it to the entire set! expression.
  ; When the identifier is used in an application position,
  ; wrap the reference in a #%expression.
  ; Expects syntax with a compiled name.
  (define (make-variable-like-reference-compiler reference-stx [setter-stx #f])
    (define transformer
      (syntax-parser
        [v:id
         (if (procedure? reference-stx)
             (reference-stx #'v)
             reference-stx)]
        [((~and set! (~literal set!)) v:id new-val)
         (cond
           [(procedure? setter-stx)
            (setter-stx this-syntax)]
           [(syntax? setter-stx)
            (syntax/loc this-syntax (setter-stx new-val))]
           [else (raise-syntax-error (syntax-e #'set!) "cannot mutate identifier" this-syntax #'v)])]
        [(v:id . args)
         (datum->syntax
          this-syntax
          #`((#%expression (call-3d-syntax #,transformer v))
             .
             args)
          this-syntax)]))
    (make-set!-transformer transformer))


  (define mutable-reference-compiler
    (make-variable-like-reference-compiler
     (lambda (id) id)
     (lambda (stx) stx)))

  (define immutable-reference-compiler
    (make-variable-like-reference-compiler
     (lambda (id) id)))

  (define suspension-property-key (gensym))

  (define/who (make-suspension stx ctx)
    (check who syntax? stx)
    (check who internal-definition-context? #:or-false ctx)

    ; wrap ctx in a pair because #f is valid as ctx but not as a syntax
    ; property value.
    (syntax-property (quasisyntax/loc stx (#%host-expression #,stx)) suspension-property-key (list ctx)))

  (define (suspension? stx)
    (not (not (and (syntax? stx)
                   (syntax-property stx suspension-property-key)))))

  ; maps binding class identifiers to promises of reference compilers.
  ; we use a promise so that the binding class form can refer to
  ; locally defined reference compilers before their definitions.
  (define current-reference-compilers
    (make-parameter (make-immutable-free-id-table)))


  (define (binding-as-rkt bclass-id description)
    (lambda (s stx)
      (define (error-as-rkt)
        (raise-syntax-error
         #f
         (string-append
          description
          " may not be used as a racket expression")
         stx))
      
      (define compile (force (free-id-table-ref
                              (current-reference-compilers) bclass-id
                              error-as-rkt)))

      ; this is an unfortunate time to have to throw the error, but because of laziness,
      ; it cannot happen sooner
      (check 'binding-class (disjoin procedure? set!-transformer?)
             #:contract "(or/c (-> syntax? syntax?) set!-transformer?)"
             compile)

      (define t (if (set!-transformer? compile)
                    (set!-transformer-procedure compile)
                    compile))
      
      (syntax-parse stx
        [v:id
         (t (compile-reference #'v))]
        [((~and set! (~literal set!)) v e)
         (if (set!-transformer? compile)
             (t (datum->syntax this-syntax (list #'set! (compile-reference #'v) #'e) this-syntax this-syntax))
             (raise-syntax-error #f (format "the reference compiler for ~a does not support set!" s) stx #'x))]
        [(v:id . rest)
         (t (datum->syntax this-syntax (cons (compile-reference #'v) #'rest) this-syntax this-syntax))])))

  #;(identifier? (promise/c reference-compiler?) -> void?)
  ; globally associates binding class with reference compiler.
  ; NOTE: this should never be called in the dynamic extent of a with-reference-compilers,
  ; or generally within a parameterization of current-reference-compilers, since it mutates the parameter.
  (define (add-global-reference-compiler! bclass compiler-promise)
    (check 'binding-class promise?
           #:contract "promise?"
           compiler-promise)
    (current-reference-compilers (free-id-table-set (current-reference-compilers) bclass compiler-promise))))

(define-syntax with-reference-compilers
  (let ([who 'with-reference-compilers])
    (syntax-parser
      [(_ ([bclass:id t-e:expr] ...) body ...+)
       (define binding-compilers
         (for/fold ([env (current-reference-compilers)])
                   ([k (attribute bclass)]
                    [t-e (attribute t-e)])
           (define v (eval-transformer t-e))
           ; TODO make this something more general once we allow arbitrary structs
           ; as compilers like match expanders.
           (check who (disjoin procedure? set!-transformer?)
                  #:contract "(or/c (-> syntax? syntax?) set!-transformer?)"
                  v)
           (free-id-table-set env k (delay v))))
       (parameterize ([current-reference-compilers binding-compilers])
         (let-values ([(_ v)
                       (syntax-local-expand-expression
                        #'(let () body ...)
                        #t)])
           v))])))

(define-syntax #%host-expression
  (let ([who '#%host-expression])
    (syntax-parser
      [(_ e:expr)
       (unless (suspension? this-syntax)
         (raise-syntax-error who "can only resume a host expansion suspension value" #'e))
       (define ctx (car (syntax-property this-syntax suspension-property-key)))
       (local-expand #'e
                     'expression
                     '()
                     ctx)])))
