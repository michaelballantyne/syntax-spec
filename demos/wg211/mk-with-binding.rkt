#lang racket

(provide (all-defined-out))

(require syntax-spec-v3
         (for-syntax syntax/parse))

;;
;; Core syntax
;;

(syntax-spec
  (binding-class term-variable)  ;; New
  (binding-class relation-name)  ;; New

  (nonterminal term
    n:number
    x:term-variable
    ((~literal quote) ())
    ((~literal cons) t1:term t2:term))

  (nonterminal goal
    succeed
    fail
    
    (== t1:term t2:term)

    (disj2 g1:goal g2:goal)
    (conj2 g1:goal g2:goal)
  
    (fresh1 (x:term-variable) b:goal)
    #:binding (scope (bind x) b)  ;; New

    (r:relation-name t:term ...+)))


;;
;; Interface macros
;;

(syntax-spec
  (host-interface/definition
    (defrel (name:relation-name x:term-variable ...) g:goal)
    #:binding [(export name) (scope (bind x) g)]  ;; New
  
    #:lhs [#'name]
    #:rhs [#''TODO])

  (host-interface/expression
    (run n:expr (q:term-variable) g:goal)
    #:binding (scope (bind q) g)  ;; New
    #''TODO))



