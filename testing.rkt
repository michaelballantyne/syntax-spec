#lang racket/base

(require "main.rkt"
         syntax/macro-testing
         rackunit
         (for-syntax racket/base syntax/parse))

(provide expand-nonterminal/datum
         (all-from-out "main.rkt"
                       rackunit
                       syntax/macro-testing)
         (for-syntax (all-from-out racket/base syntax/parse)))

(define-syntax expand-nonterminal/datum
  (syntax-parser
    [(_ nonterm:id form)
     #'(phase1-eval ((nonterminal-expander nonterm) #'form))]))