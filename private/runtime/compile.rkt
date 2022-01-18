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
    (match-define (suspension stx ctx) (syntax-e s))
    #`(expand-suspension #,(closed-suspension stx ctx (binding-compilers))))

  (define (binding-as-rkt bclass-id description)
    (lambda (s stx)
      (let ([compile (free-id-table-ref (binding-compilers) bclass-id)])
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
       (local-expand stx
                     'expression
                     '()
                     ctx))]))

(begin-for-syntax
  (define (compile-binder! table id)
    (unless (mutable-free-id-table? table)
      (raise-argument-error
       'add-fresh-name!
       "mutable-free-id-table?"
       table))
    (unless (identifier? id)
      (raise-argument-error
       'add-fresh-name!
       "identifier?"
       id))
    
    (define result (generate-temporary id))

    (free-id-table-set! table
                        (flip-intro-scope id)
                        result)
  
    (flip-intro-scope
     result))

  (define (compile-reference table id)
    (syntax-local-get-shadower
     (flip-intro-scope
      (free-id-table-ref table
                         (flip-intro-scope id))))))