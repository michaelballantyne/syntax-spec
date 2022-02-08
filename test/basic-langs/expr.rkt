#lang racket/base

(require "../../testing.rkt")

(define-hosted-syntaxes
  (binding-class var #:description "expr language variable")
  
  (nonterminal expr
    #:description "simple expr language expression"

    n:number
    v:var
    (+ e1:expr e2:expr)
      
    (let ([v:var e:expr] ...) b:expr)
    #:binding [e {(bind v) b}]
      
    (let* (b:binding ...) e:expr)
    #:binding (nest b e))

  (nesting-nonterminal binding (nested)
    #:description "let* binding group"
    
    [v:var e:expr]
    #:binding [e {(bind v) nested}]))

(check-equal?
 (expand-nonterminal/datum expr
   (let ([x 5]) (let ([x (+ x 1)]) x)))
 '(let ([x 5]) (let ([x (+ x 1)]) x)))

(check-equal?
 (expand-nonterminal/datum expr
   (let* ([x 5] [x (+ x 1)]) x))
 '(let* ([x 5] [x (+ x 1)]) x))

(check-exn
 #rx"y: not bound as expr language variable"
 (lambda ()
   (convert-compile-time-error
    (expand-nonterminal/datum expr (let* ([x 5]) y)))))
