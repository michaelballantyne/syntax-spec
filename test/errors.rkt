#lang racket/base

(require "../main.rkt"
         (for-syntax racket/base syntax/parse)
         syntax/macro-testing
         rackunit)

(define-hosted-syntaxes
  (nonterminal expr1
               n:number)
  (nonterminal expr2
               #:description "DSL expression"
               n:number))

(check-exn
 #rx"foo: expected expr1"
 (lambda ()
   (phase1-eval
    ((nonterminal-expander expr1) #'foo)
    #:catch? #t)))

(check-exn
 #rx"foo: expected DSL expression"
 (lambda ()
   (phase1-eval
    ((nonterminal-expander expr2) #'foo)
    #:catch? #t)))