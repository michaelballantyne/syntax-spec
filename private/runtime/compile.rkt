#lang racket/base

(provide (for-syntax binding-as-rkt
                     make-suspension
  
                     resume-host-expansion))

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
    (check who internal-definition-context? #:or-false ctx)

    ; wrap ctx in a pair because #f is valid as ctx but not as a syntax
    ; property value.
    (syntax-property stx suspension-property-key (list ctx)))

  (define (suspension? stx)
    (not (not (and (syntax? stx)
                   (syntax-property stx suspension-property-key)))))
  
  (struct closed-suspension [stx ctx compile])

  (define current-binding-compilers
    (make-parameter #f))


  (define-syntax resume-host-expansion
    (syntax-parser
      [(_ stx-e:expr
          (~optional (~seq #:reference-compilers
                           ([bclass:id t-e:expr] ...))
                     #:defaults ([(bclass 1) '()] [(t-e 1) '()])))
       #'(resume-host-expansion-rt
          stx-e
          (list (quote-syntax bclass) ...) (list t-e ...))]))
  
  (define/who (resume-host-expansion-rt stx ks vs)
    (check who suspension? stx)

    (define binding-compilers
      (for/fold ([env (make-immutable-free-id-table)])
                ([k ks]
                 [v vs])
        (check who procedure? v)
        (free-id-table-set env k v)))
    
    (define ctx (car (syntax-property stx suspension-property-key)))
    #`(expand-suspension #,(closed-suspension stx ctx binding-compilers)))


  (define (binding-as-rkt bclass-id description)
    (lambda (s stx)
      (define (error-as-rkt)
        (raise-syntax-error
         #f
         (string-append
          description
          " may not be used as a racket expression")
         stx))
      
      (when (not (current-binding-compilers))
        (error-as-rkt))
      
      (let ([compile (free-id-table-ref
                      (current-binding-compilers) bclass-id
                      error-as-rkt)])
        (compile stx)))))



(define-syntax expand-suspension
  (syntax-parser
    [(_ s)
     (match-define (closed-suspension stx ctx binding-compilers) (syntax-e #'s))
     (parameterize ([current-binding-compilers binding-compilers])
       (local-expand (flip-intro-scope stx)
                     'expression
                     '()
                     ctx))]))
