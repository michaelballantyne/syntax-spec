#lang info

(define name "syntax-spec")
(define collection "syntax-spec")
(define version "0.1")
(define deps '("base"
               "version-case"
               "ee-lib"
               "paren-shape"
               "rackunit-lib"))
(define build-deps '("racket-doc" "scribble-lib" "drracket"))
(define scribblings '(("scribblings/main.scrbl" (multi-page) (experimental))))
(define compile-omit-paths '("design" "demos"))
(define test-omit-paths '("design" "demos"))
