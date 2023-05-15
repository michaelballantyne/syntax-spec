#lang racket/base

(provide
 (all-defined-out)
 (for-syntax peg-literals))

(require
  (for-syntax
   syntax/parse))

(begin-for-syntax
  (define-literal-set peg-literals
    #:datum-literals
    (eps
     seq
     alt
     plain-alt
     ?
     *
     repeat
     !
     :
     bind
     =>
     text
     char
     token
     :src-span
     src-span
     #%nonterm-ref
     )
    ()))
