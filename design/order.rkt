#lang racket/base

(require "../main.rkt"
         rackunit
         syntax/macro-testing
         (for-syntax racket/base syntax/parse racket/pretty))

(define-hosted-syntaxes
  (binding-class var "expr language variable")
  
  (nonterminal expr
    #:description "simple expr language expression"

    n:number
    v:var
    (+ e1:expr e2:expr)
      
    (let ([v:var e:expr] ...) b:expr)
    ;; should be an error; binds should be written first.
    #:binding [e {b (! v)}]))


(define-syntax exprlang
  (syntax-parser
    [(_ e) #`'#,((nonterminal-expander expr) #'e)]))
