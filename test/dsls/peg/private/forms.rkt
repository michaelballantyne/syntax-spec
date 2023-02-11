#lang racket/base

(provide
 (all-defined-out)
 (for-syntax peg-literals))

(require
  ee-lib/define
  (for-syntax
   racket/base))
         
(define-literal-forms
  peg-literals
  "peg forms cannot be used as racket expressions"
  (eps
   seq
   alt
   plain-alt
   *
   !
   :
   =>
   text
   char
   token  ; semantics in the paper isn't enough, because it needs to support values and srclocs too.
   :src-span
   ))
