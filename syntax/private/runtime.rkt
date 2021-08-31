#lang racket/base

(provide nonterminal-expander-rt)

(require
  "../../syntax-spec/expand.rkt"
  "../../binding-spec/expand.rkt")

(define (nonterminal-expander-rt stx
                                 stx-spec
                                 binding-spec)
  (define vmap (deconstruct stx stx-spec))
  (define vmap^ (simple-expand binding-spec vmap))
  (reconstruct vmap^ stx stx-spec))