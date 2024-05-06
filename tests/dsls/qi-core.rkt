#lang racket/base

(require "../../testing.rkt")

(syntax-spec
  (nonterminal/nesting binding-floe (nested)
    (as v:racket-var)
    #:binding (scope (bind v) nested)

    (thread f:binding-floe ...)
    #:binding (nest f nested)

    f:simple-floe
    #:binding [f nested])

  (nonterminal simple-floe
    v:racket-var
    (gen n:number)
    (or f:floe ...))

  (nonterminal floe
    f:binding-floe
    #:binding (nest-one f [])))

(void
 (expand-nonterminal/datum floe
                           (thread (or (gen 1) (gen 2)) (as v) v)))

(check-syntax-error
 #rx"v: not bound as racket variable"
 (expand-nonterminal/datum floe
   (thread (or (as v) v) (gen 1))))

(check-syntax-error
 #rx"v: not bound as racket variable"
 (expand-nonterminal/datum floe
   (thread (or (as v) (gen 1)) v)))
   
