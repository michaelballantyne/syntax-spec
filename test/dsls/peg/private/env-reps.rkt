#lang racket/base

(provide (all-defined-out))

(require racket/generic)

(define-generics peg-non-terminal)
(struct peg-non-terminal-rep ()
  #:methods gen:peg-non-terminal [])

(define-generics peg-macro
  (peg-macro-transform peg-macro stx))
(struct peg-macro-rep (procedure)
  #:extra-constructor-name peg-macro
  #:methods gen:peg-macro
  [(define (peg-macro-transform s stx)
     ((peg-macro-rep-procedure s) stx))])
