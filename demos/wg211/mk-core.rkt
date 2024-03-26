#lang racket

(provide (all-defined-out))

(require syntax-spec
         (for-syntax syntax/parse))

;;
;; Core syntax
;;

(syntax-spec
  (nonterminal term
    n:number
    x:id
    ((~literal quote) ())
    ((~literal cons) t1:term t2:term))

  (nonterminal goal
    succeed
    fail
    
    (== t1:term t2:term)

    (disj2 g1:goal g2:goal)
    (conj2 g1:goal g2:goal)
  
    (fresh1 (x:id) b:goal)

    (r:id t:term ...+)))

;;
;; Interface macros
;;

(syntax-spec
  (host-interface/definition
    (defrel (name:id x:id ...)
      g:goal)
  
    #:lhs [#'name]
    #:rhs [#''TODO])

  (host-interface/expression
    (run n:racket-expr (q:id) g:goal)
    #''TODO))



