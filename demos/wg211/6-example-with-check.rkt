#lang racket/base

(module def racket
  (require "mk.rkt")
  (provide appendo)
 
  (defrel (appendo l1 l2 l3)
    (conde
      [(== l1 '()) (== l2 l3)]
      [(fresh (first rest res)
         (== l1 (cons first rest))
         (== l3 (cons first res))
         (appendo rest l2 res))])))

(module use racket
  (require "mk.rkt" (submod ".." def))
  
  (run* (l1 l2)
    (appendo l1 l2 (list 1 2 3 4))))  ;; What if I make an arity mistake?

(require 'use)