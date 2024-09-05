#lang racket/base

(require hosted-minikanren
         hosted-minikanren/racket-matche
         hosted-minikanren/inspect)


(defrel (appendo l1 l2 l3)
  (disj
   (conj (== l1 '()) (== l2 l3))
   (fresh (first rest res)
     (conj
      (== l1 (cons first rest))
      (== l3 (cons first res))
      (appendo rest l2 res)))))

(defrel/matche (appendo/m l1 l2 l3)
  [('() l l)]
  [((cons first rest)
    l2
    (cons first res))
   (appendo rest l2 res)])


#;(run 3 (n)
  (peano n))


(begin
  (print-relation-code appendo/m)
  (print-relation-code/after-dead-code appendo/m))