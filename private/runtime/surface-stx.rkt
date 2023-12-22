#lang racket/base

(provide syntax-surface-stx
         annotate-surface-stx)

(define surface-prop 'surface)

(define (syntax-surface-stx stx)
  (syntax-property stx surface-prop))

(define (annotate-surface-stx stx-expanded stx-surface)
  (syntax-property stx-expanded surface-prop stx-surface))