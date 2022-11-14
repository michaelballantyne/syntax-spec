#lang racket/base

(require "main.rkt"
         syntax/macro-testing
         rackunit
         (for-syntax racket/base syntax/parse racket/syntax))

(provide expand-nonterminal/datum
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
