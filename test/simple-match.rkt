#lang racket/base

(require "../main.rkt"
         rackunit
         (for-syntax racket/base syntax/parse racket/pretty))

(define-hosted-syntaxes
  (binding-class var "mylang variable")

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
               #:binding {p rhs})

  (nonterminal pat
               #:description "mylang match pattern"
               
               v:var
               #:binding (! v) ; should this be an error, with an export required? Check in compile or runtime?

               (pempty)
               
               (pcons p1:pat p2:pat)))

;; simulated interface macro
(define-syntax mylang-expr
  (syntax-parser
    [(_ e) #`'#,((nonterminal-expander expr) #'e)]))

(define-syntax define*
  (mylang-macro
   (syntax-parser
     [(_ v e)
      #'(define*-values (v) e)])))

;; tests
(check-equal?
 (mylang-expr
  (match (cons 1 (cons 2 (empty)))
    [(pcons a (pcons b (pempty)))
     (+ a b)]))
 '(match (cons 1 (cons 2 (empty)))
    [(pcons a (pcons b (pempty)))
     (+ a b)]))
