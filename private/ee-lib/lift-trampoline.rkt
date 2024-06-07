#lang racket/base

(provide wrap-lift-trampoline
         trampoline-lift!
         trampoline-lift-context?)

(require racket/control
         racket/private/check)

(module macro racket/base
  (require (for-syntax racket/base))
  (provide continue-trampoline)
  (define-syntax (continue-trampoline stx)
    ((syntax-e (cadr (syntax->list stx))))))

(require (for-template racket/base 'macro))

(define lift-tag (make-continuation-prompt-tag))

(define/who (trampoline-lift! stx)
  (check who syntax? stx)

  (unless (trampoline-lift-context?)
    (error who "not in trampoline lift context"))
  
  (control-at
   lift-tag k
   #`(begin #,stx
            (continue-trampoline
             #,(lambda () (prompt-at lift-tag (k)))))))

(define (trampoline-lift-context?)
  (continuation-prompt-available? lift-tag))

(define/who (wrap-lift-trampoline transformer)
  (check who procedure? transformer)
  
  (lambda (stx)
    (prompt-at lift-tag (transformer stx))))