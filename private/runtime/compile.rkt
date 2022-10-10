#lang racket/base

(provide #%host-expression
         with-reference-compilers
         (for-syntax binding-as-rkt
                     make-suspension

                     mutable-reference-compiler
                     immutable-reference-compiler))

(require
  (for-syntax
   racket/base
   racket/function
   racket/private/check
   racket/match
   syntax/parse
   "../syntax/env-reps.rkt"
   syntax/id-table
   syntax/transformer
   racket/syntax
   ee-lib)
  (for-meta 2
            racket/base
            syntax/parse
            "../syntax/env-reps.rkt"
            ee-lib))

(begin-for-syntax  
  (define mutable-reference-compiler
    (make-variable-like-transformer
     compile-reference
     (syntax-parser
       [((~literal set!) x:id e:expr)
        #:with x^ (compile-reference #'x)
        #'(set! x^ e)])))

  (define immutable-reference-compiler
    (make-variable-like-transformer
     compile-reference))
  
  (define suspension-property-key (gensym))

  (define/who (make-suspension stx ctx)
    (check who syntax? stx)
    (check who internal-definition-context? #:or-false ctx)

    ; wrap ctx in a pair because #f is valid as ctx but not as a syntax
    ; property value.
    (syntax-property #`(#%host-expression #,stx) suspension-property-key (list ctx)))

  (define (suspension? stx)
    (not (not (and (syntax? stx)
                   (syntax-property stx suspension-property-key)))))

  (define current-reference-compilers
    (make-parameter #f))


  (define (binding-as-rkt bclass-id description)
    (lambda (s stx)
      (define (error-as-rkt)
        (raise-syntax-error
         #f
         (string-append
          description
          " may not be used as a racket expression")
         stx))
      
      (when (not (current-reference-compilers))
        (error-as-rkt))
      
      (let ([compile (free-id-table-ref
                      (current-reference-compilers) bclass-id
                      error-as-rkt)])
        (if (set!-transformer? compile)
            ((set!-transformer-procedure compile) stx)
            (syntax-parse stx
              [((~literal set!) x:id e:expr)
               (raise-syntax-error #f (format "the reference compiler for ~a does not support set!" s) stx #'x)]
              [_ (compile stx)]))))))

(define-syntax with-reference-compilers
  (let ([who 'with-reference-compilers])
    (syntax-parser
      [(_ ([bclass:id t-e:expr] ...) body ...+)
       (define binding-compilers
         (for/fold ([env (make-immutable-free-id-table)])
                   ([k (attribute bclass)]
                    [t-e (attribute t-e)])
           (define v (eval-transformer t-e))
           ; TODO make this something more general once we allow arbitrary structs
           ; as compilers like match expanders.
           (check who (disjoin procedure? set!-transformer?)
                  #:contract "(or/c (-> syntax? syntax?) set!-transformer?)"
                  v)
           (free-id-table-set env k v)))
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
