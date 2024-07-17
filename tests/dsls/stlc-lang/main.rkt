#lang racket

; A little #lang wrapper around stlc.
; The first form of a program is Racket, the rest are stlc.
; The first form is intended to be used to provide stlc-defined identifiers.

(require "../simply-typed-lambda-calculus.rkt")
(provide (rename-out [stlc-module-begin #%module-begin])
         (all-from-out "../simply-typed-lambda-calculus.rkt")
         (except-out (all-from-out racket)
                     #%module-begin))

(define-syntax-rule
  (stlc-module-begin racket-expr stlc-expr ...)
  (#%module-begin
   racket-expr
   (stlc stlc-expr ...)))
