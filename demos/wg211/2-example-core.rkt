#lang racket/base

(require "mk-core.rkt")

(defrel (appendo l1 l2 l3)
  (disj2
   (conj2 (== l1 '()) (== l2 l3))
   (fresh1 (first)
     (fresh1 (rest)
       (fresh1 (res)
         (conj2
          (conj2
           (== l1 (cons first rest))
           (== l3 (cons first res)))
          (appendo rest l2 res)))))))

(run 5 (q)
  (fresh1 (l1)
    (fresh1 (l2)
      (conj2 (== q (cons l1 (cons l2 '())))
             (appendo l1 l2 (cons 1 (cons 2 (cons 3 (cons 4 '())))))))))




;; What if I make a grammar mistake?

#;(run 1 (q)
    (fresh1 (q) q))