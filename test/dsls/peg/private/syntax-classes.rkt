#lang racket/base

(provide (all-defined-out))

(require
  syntax/parse
  ee-lib
  "env-reps.rkt")

(define-syntax-class peg
  #:description "PEG expression"
  (pattern e))

(define-syntax-class nonterm-id
  #:description "PEG non-terminal name"
  (pattern n:id
           #:fail-unless
           (lookup #'n peg-non-terminal?)
           "not bound as a peg"))
