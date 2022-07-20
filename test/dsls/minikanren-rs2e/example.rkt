#lang racket/base

(module a racket/base
  (require "mk.rkt")
  (provide appendo)

  (defrel (appendo l1 l2 l3)
    (conde
     [(== l1 '()) (== l2 l3)]
     [(fresh (first rest res)
        (== l1 (cons first rest))
        (== l3 (cons first res))
        (appendo rest l2 res))])))

(module b racket/base
  (require "mk.rkt" (submod ".." a))

  (run* (l1 l2)
    (appendo l1 l2 '(1 2 3 4)))

  (let ()
    (define (f) (run 1 (q) (is-five q)))
    (defrel (is-five a)
      (== a 5))
    
    (f))

  (let ()
    (defrel (eveno x)
      (conde
       [(== x 'z)]
       [(fresh (x-1)
          (== x `(s ,x-1))
          (oddo x-1))]))
    
    (defrel (oddo x)
      (fresh (x-1)
        (== x `(s ,x-1))
        (eveno x-1)))
    
    (run 4 (q)
      (eveno q)))

  (run 1 (q)
    (fresh (x)
      (== q `(,x . 5))
      (project (q)
               (begin
                 (displayln q)
                 (goal-expression (project (q)
                                           (goal-expression
                                            (== x 1)))))))))

(require 'b)


