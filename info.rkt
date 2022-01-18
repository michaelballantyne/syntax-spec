#lang info

(define name "bindingspec")
(define version "0.1")
(define deps '("ee-lib"
               "paren-shape"
               "rackunit-lib"))
(define build-deps '("racket-doc" "scribble-lib" "drracket"))
(define scribblings '())
(define compile-omit-paths '("design"))
(define test-omit-paths '("design"))
