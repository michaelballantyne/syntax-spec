#lang racket/base

(require (rename-in bindingspec [~> ~*>] )
         (for-syntax racket/base))

(define e 5)

(define-hosted-syntaxes
  (nonterminal expr
    (foo)
    (~literal e)))

(define-host-interface/expression
  (dsl e:expr)
  #'(quote-syntax e))

(dsl e)