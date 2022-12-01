#lang racket/base

(require "main.rkt"
         syntax/macro-testing
         rackunit
         racket/exn
         (for-syntax racket/base syntax/parse racket/syntax))

(provide expand-nonterminal/datum
         check-decl-error
         check-phase1-error
         check-syntax-error
         
         (all-from-out "main.rkt"
                       rackunit
                       syntax/macro-testing)
         (for-syntax (all-from-out racket/base syntax/parse)))

(define-syntax expand-nonterminal/datum
  (syntax-parser
    [(_ nonterm:id form)
     (define/syntax-parse ctx this-syntax)
     #'(phase1-eval
        (parameterize ([current-syntax-context #'ctx])
          ((nonterminal-expander nonterm) #'form))
        #:catch? #t)]))

(define ((check-formatted-error-matches rx) exn)
  ;; I previously used exn->string, but that raised an error
  ;; re: writing special values when handling an ambiguous binding
  ;; error.
  (regexp-match? rx (exn-message exn)))

(define-syntax-rule (check-decl-error rx decl-stx)
  (check-exn
   (check-formatted-error-matches rx)
   (lambda ()
     (eval-syntax #`(module m racket/base
                      (require "../main.rkt")
                      decl-stx)))))

(define-syntax-rule (check-phase1-error rx e)
  (check-exn
   (check-formatted-error-matches rx)
   (lambda () (phase1-eval e #:catch? #t))))

(define-syntax-rule (check-syntax-error rx e)
  (check-exn
   (check-formatted-error-matches rx)
   (lambda () (convert-compile-time-error e))))
