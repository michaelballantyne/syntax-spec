#lang racket/base

(require "08-mk-compiled.rkt")

(defrel (appendo l1 l2 l3)
  (conde
    [(== l1 '()) (== l2 l3)]
    [(fresh (first rest res)
       (== l1 (cons first rest))
       (== l3 (cons first res))
       (appendo rest l2 res))]))

(run* (l1 l2)
  (appendo l1 l2 (list 1 2 3 4)))

;; What if I call a relation with the wrong number of arguments?
(run* (l1 l2)
  (appendo l1 (list 1 2 3 4)))