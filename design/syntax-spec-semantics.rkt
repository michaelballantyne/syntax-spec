#lang racket/base

(define (apply-nonterminal nonterminal stx scope)
  ;; Matches stx againts nonterminal productions and calls back in
  ;; to eval-bspec
  ...)

;; BSpec, PEnv -> ScopeGraphStatement
(define (eval-bspec spec env scope)
  (match spec
    ;; scope
    [`(scope ,spec2)
     (fresh-scope scope2
       (edge scope2 (-> parent) scope)
       (eval-bspec spec2 env scope2))]
    ;; binding
    [`(bind ,v) #:when (binding-class? (lookup-type v env))
     ;; TODO: how to encode binding class in scope graphs?
     (edge s (-> binding) (lookup-syntax v env))]
    ;; variable reference
    [v #:when (binding-class? (lookup-type v env))
     ;; TODO: how to encode binding class check?
     (edge (lookup-syntax v env) (-> reference) s)]
    ;; simple subexpression
    [v #:when (nonterminal? (lookup-type v env))
     (apply-nonterminal (lookup-type v env) (lookup-syntax v env) scope)]))
