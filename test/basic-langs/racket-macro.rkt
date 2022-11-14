#lang racket/base

(require "../../testing.rkt")


(define-hosted-syntaxes
  (extension-class expr-macro)

  (binding-class var #:description "expr language variable")
  
  (nonterminal expr
    #:description "simple expr language expression"
    #:allow-extension (expr-macro racket-macro)

    n:number
    v:var
    (+ e1:expr e2:expr)
      
    (let ([v:var e:expr] ...) b:expr)
    #:binding [e {(bind v) b}]))

(define-syntax let*
  (syntax-rules ()
    [(let* () body) body]
    [(let* ([x rhs] binding ...) body)
     (let ([x rhs])
       (let* (binding ...) body))]))

(define-syntax expr-identity
  (expr-macro (syntax-rules () [(_ e) e])))

(check-equal?
 (expand-nonterminal/datum expr
   (let ([x 5]) (let ([x (+ x 1)]) x)))
 '(let ([x 5]) (let ([x (+ x 1)]) x)))

(check-equal?
 (expand-nonterminal/datum expr
   (let* ([x 5] [x (+ x 1)]) (expr-identity x)))
 '(let ([x 5]) (let ([x (+ x 1)]) x)))
