#lang info

(define name "syntax-spec-dev")
(define collection "syntax-spec-dev")
(define version "0.1")
(define license '(Apache-2.0 OR MIT))
(define deps '("base"
               "version-case"
               "syntax-classes-lib"
               "rackunit-lib"))
(define build-deps '("racket-doc" "scribble-lib" "drracket" "typed-racket-lib" "ocular-patdown"))
(define scribblings '(("scribblings/main.scrbl" (multi-page) (experimental) "syntax-spec-dev")))
(define compile-omit-paths '("design" "demos"))
(define test-omit-paths '("scribblings" "design" "demos"))
