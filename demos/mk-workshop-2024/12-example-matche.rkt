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

;; We can define the same relation using a pattern matching extension.

(defrel/matche (appendo/m l1 l2 l3)
  [('() l l)]
  [((cons first rest) l2 (cons first res))
   (appendo/m rest l2 res)])

(begin
  ;; Shows the macro-expanded code that the miniKanren compiler receives.
  #;(print-relation-code appendo/m)

  ;; Shows the optimized code after constant prop and dead code elim.
  (print-relation-code/after-dead-code appendo/m))