#lang racket/base

#;(run* (q)
    (and
     (exists (x)
       (or
        (== q (cons 1 x))
        (== q (cons 2 x))
        (== q (cons 3 x))))
     (exists (y)
       (or
        (== q (cons y 4))
        (== q (cons y 5))))))
;; =>
#;'((1 . 4) (1 . 5)
    (2 . 4) (2 . 5)
    (3 . 4) (3 . 5))



;; Syntax definition

(require bindingspec)

(syntax-spec
  (binding-class term-variable)

  (nonterminal goal    
    (== t1:term t2:term)
    (and2 g1:goal g2:goal)
    (or2 g1:goal g2:goal)
    
    (exists (x:term-variable ...) b:goal))

  (nonterminal term    
    n:number
    x:term-variable
    (cons t1:term t2:term)))


