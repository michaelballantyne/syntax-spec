#lang racket/base

(provide
 (for-syntax
  suspension
  resume-host-expansion
  with-binding-compilers
  binding-as-rkt
  compile-reference
  compile-binder!))

(require
  (for-syntax
   racket/base
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
  (struct suspension [stx ctx]
    #:property prop:procedure
    (lambda (s stx)
      (raise-syntax-error #f "use resume-host-expansion")))
  
  (struct closed-suspension [stx ctx compile])

  (define binding-compilers
    (make-parameter (make-immutable-free-id-table
                     #:phase (+ 1 (syntax-local-phase-level)))))

  (define (resume-host-expansion s)
    (when (not (and (syntax? s) (suspension? (syntax-e s))))
      (raise-argument-error 'resume-host-expansion "host-expand-suspension?" s))
    (match-define (suspension stx ctx) (syntax-e s))
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
                      (free-id-table-set env k v))])
      (f)))

  (define-syntax with-binding-compilers
    (syntax-parser
      [(_ ([bclass e] ...) body ...)
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
