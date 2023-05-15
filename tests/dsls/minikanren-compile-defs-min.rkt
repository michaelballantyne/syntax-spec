#lang racket/base

(require "minikanren-compile-defs.rkt")

(#%expression (run 1 (q) (oddo 5) (eveno 4)))

(mk-defs
 (define-relation2 (eveno n)
   (oddo n))
 (define-relation2 (oddo n)
   (eveno n)))