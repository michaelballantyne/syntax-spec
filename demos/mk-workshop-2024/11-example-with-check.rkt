#lang racket/base

(module def racket
  (require "10-mk-with-check.rkt")
  (provide appendo)
 
  (defrel (appendo l1 l2 l3)
    (conde
      [(== l1 '()) (== l2 l3)]
      [(fresh (first rest res)
         (== l1 (cons first rest))
         (== l3 (cons first res))
         (appendo rest l2 res))])))

(module use racket
  (require "10-mk-with-check.rkt"
           (submod ".." def))
  
  (run* (l1 l2)
    ;; Now arity mistakes get friendly errors.
    (appendo l1 (list 1 2 3 4))))  

(require 'use)