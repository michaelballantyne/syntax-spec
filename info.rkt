#lang info

(define name "syntax-spec-v1")
(define version "1.0")
(define deps '("base"
               "version-case"
               "ee-lib"
               "syntax-classes-lib"
               "paren-shape"
               "rackunit-lib"))
(define build-deps '("racket-doc" "scribble-lib" "drracket"))
(define scribblings '(("scribblings/main.scrbl" (multi-page) (experimental))))
(define compile-omit-paths '("design" "demos"))
(define test-omit-paths '("design" "demos"))
