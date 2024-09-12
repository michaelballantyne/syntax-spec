#lang info

(define name "syntax-spec-v2")
(define collection "syntax-spec-v2")
(define version "2.0")
(define license '(Apache-2.0 OR MIT))
(define deps '("base"
               "version-case"
               "syntax-classes-lib"
               "rackunit-lib"))
(define build-deps '("racket-doc" "scribble-lib" "drracket" "typed-racket-lib"))
(define scribblings '(("scribblings/main.scrbl" (multi-page) (experimental) "syntax-spec-v2")))

(define compile-omit-paths '("design" "demos"))
(define test-omit-paths '("design" "demos"))
