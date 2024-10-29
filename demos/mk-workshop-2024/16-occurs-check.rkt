#lang racket/base

(require hosted-minikanren
         hosted-minikanren/inspect)

;; The occurs-check

(run 1 (q)
  (== q (list q)))


;; Optimizing away the occurs-check

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





;; What if the representation of logic variables were exposed?

#;(defrel (r3 x y)
  (fresh (a b)
    (== x (cons '5 '6))
    (goal-from-expression
     (void (vector-set! x 1 (list y))))
    (== a b)
    (== y x)))