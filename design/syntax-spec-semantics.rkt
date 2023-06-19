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
    [`(bind ,v) #:when (variable? (lookup v env))
     (edge s (-> binding) (lookup v env))]
    ;; variable reference
    [v          #:when (variable? (lookup v env))
     (edge (lookup v env) (-> reference) s)]
    ;; simple subexpression
    [v          #:when (subexpression? (lookup v env))
     (let-values ([(stx nonterminal) (lookup v env)])
       (apply-nonterminal nonterminal stx scope))]))
