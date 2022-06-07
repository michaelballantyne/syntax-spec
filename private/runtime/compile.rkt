#lang racket/base

(provide (for-syntax binding-as-rkt
                     make-suspension
  
                     resume-host-expansion
                     with-reference-compilers
                     ))

(require
  (for-syntax
   racket/base
   racket/private/check
   racket/match
   syntax/parse
   "../syntax/env-reps.rkt"
   syntax/id-table
   racket/syntax
   ee-lib)
  (for-meta 2
            racket/base
            syntax/parse
            "../syntax/env-reps.rkt"
            ee-lib))

(begin-for-syntax
  (define suspension-property-key (gensym))

  (define/who (make-suspension stx ctx)
    (check who syntax? stx)
    (check who internal-definition-context? ctx)
    
    (syntax-property stx suspension-property-key ctx))

  (define (suspension? stx)
    (not (not (and (syntax? stx)
                   (syntax-property stx suspension-property-key)))))
  
  (struct closed-suspension [stx ctx compile])

  (define binding-compilers
    (make-parameter (make-immutable-free-id-table
                     #:phase (+ 1 (syntax-local-phase-level)))))

  (define/who (resume-host-expansion stx)
    (check who suspension? stx)

    (define ctx (syntax-property stx suspension-property-key))
    #`(expand-suspension #,(closed-suspension stx ctx (binding-compilers))))

  (define (binding-as-rkt bclass-id description)
    (lambda (s stx)
      (let ([compile (free-id-table-ref
                      (binding-compilers) bclass-id
                      (lambda ()
                        (raise-syntax-error
                         #f
                         (format "binding compiler not defined for binding class ~a"
                                 bclass-id)
                         stx)))])
        (if compile
            (compile stx)
            (raise-syntax-error
             #f
             (string-append
              description
              " may not be used as a racket expression")
             stx)))))

  (define (extend-binding-compilers ks vs f)
    (parameterize ([binding-compilers
                    (for/fold ([env (binding-compilers)])
                              ([k ks]
                               [v vs])
                      (check 'with-reference-compilers procedure? v)
                      (free-id-table-set env k v))])
      (f)))

  (define-syntax with-reference-compilers
    (syntax-parser
      [(_ ([bclass:id e] ...) body ...+)
       #'(extend-binding-compilers
          (list (quote-syntax bclass) ...) (list e ...)
          (lambda () body ...))])))

(define-syntax expand-suspension
  (syntax-parser
    [(_ s)
     (match-define (closed-suspension stx ctx compile) (syntax-e #'s))
     (parameterize ([binding-compilers compile])
       (local-expand (flip-intro-scope stx)
                     'expression
                     '()
                     ctx))]))
