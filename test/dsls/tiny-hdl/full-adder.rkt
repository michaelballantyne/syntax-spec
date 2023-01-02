#lang racket/base

(require "hdl.rkt" "half-adder.rkt")

(begin-tiny-hdl
  (entity full-adder ([input a] [input b] [input ci] [output s] [output co]))

  (architecture full-adder-arch full-adder
                (instance h1 half-adder-arch)
                (instance h2 half-adder-arch)
                (assign (h1 a) a)
                (assign (h1 b) b)
                (assign (h2 a) (h1 s))
                (assign (h2 b) ci)
                (assign s (h2 s))
                (assign co (or (h1 co) (h2 co)))))

(print-truth-table
 full-adder-arch (a b ci) (s co)
 (#f #f #f)
 (#f #f #t)
 (#f #t #f)
 (#f #t #t)
 (#t #f #f)
 (#t #f #t)
 (#t #t #f)
 (#t #t #t))