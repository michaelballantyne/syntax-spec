#lang racket/base

(require "../../testing.rkt")

(syntax-spec
  (binding-class var #:description "mylang variable")

  (extension-class mylang-macro)

  (nonterminal expr
    #:description "mylang expression"
    #:allow-extension mylang-macro

    v:var
               
    n:number
    (+ e1:expr e2:expr)
               
    (empty)
    (cons e1:expr e2:expr)

    ;; note that lack of explicit ... in binding specs means
    ;; we need to use a match-clause nonterminal
    (match e:expr c:match-clause ...))

  (nonterminal match-clause
    #:description "mylang match clause"

    [p:pat rhs:expr]
    #:binding (nest p rhs))

  (nonterminal/nesting pat (nested)
    #:description "mylang match pattern"
               
    v:var
    #:binding (scope (bind v) nested)
    
    (pempty)
               
    (pcons p1:pat p2:pat)
    #:binding (nest p1 (nest p2 nested))))

(define-syntax define*
  (mylang-macro
   (syntax-parser
     [(_ v e)
      #'(define*-values (v) e)])))

;; tests
(check-equal?
 (expand-nonterminal/datum expr
   (match (cons 1 (cons 2 (empty)))
     [(pcons a (pcons b (pempty)))
      (+ a b)]))
 '(match (cons 1 (cons 2 (empty)))
    [(pcons a (pcons b (pempty)))
     (+ a b)]))
