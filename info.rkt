#lang info

(define name "syntax-spec-v1")
(define version "1.0")
(define collection "syntax-spec-v1")
(define license '(Apache-2.0 OR MIT))
(define deps '("base"
               "version-case"
               "ee-lib"
               "syntax-classes-lib"
               "rackunit-lib"))
(define build-deps '("racket-doc" "scribble-lib" "drracket"))
(define scribblings '(("scribblings/main.scrbl" (multi-page) (experimental))))
(define compile-omit-paths '("design" "demos"))
(define test-omit-paths '("design" "demos"))
