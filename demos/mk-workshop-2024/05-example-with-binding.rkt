#lang racket/base

(require "04-mk-with-binding.rkt")

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


;; DrRacket understands binding structure now,
;; and unbound references are errors.

(run 1 (q)
  (fresh1 (x)
    (fresh1 (x)
      (== q x))))