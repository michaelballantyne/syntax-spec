#lang racket/base

(require hosted-minikanren
         hosted-minikanren/inspect)

(defrel (r1 a b)
  (fresh (x y)
    (== a (list 1 x))
    (== b (list 1 y))
    (== x y)))

#;(print-relation-code/after-occurs-check r1)





#;(defrel (r2 x y)
  (fresh (a b)
    (== x (cons '5 '6))
    (goal-from-expression
     (void #| unknown racket code |#))
    (== a b)
    (== y x)))

#;(print-relation-code/after-occurs-check r2)
  