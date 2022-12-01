#lang racket/base

(require "../../testing.rkt")

(define-hosted-syntaxes
  (binding-class qi-var)
  
  (nesting-nonterminal binding-floe (nested)
    (as v:qi-var)
    #:binding {(bind v) nested}

    (thread f:binding-floe ...)
    #:binding (nest f nested)

    f:simple-floe
    #:binding [f nested])

  (nonterminal simple-floe
    v:qi-var
    (gen n:number)
    (or f:floe ...))

  (nonterminal floe
    f:binding-floe
    #:binding (nest-one f [])))

(expand-nonterminal/datum floe
   (thread (or (gen 1) (gen 2)) (as v) v))

(check-syntax-error
 #rx"v: not bound as qi-var"
 (expand-nonterminal/datum floe
   (thread (or (as v) v) (gen 1))))

(check-syntax-error
 #rx"v: not bound as qi-var"
 (expand-nonterminal/datum floe
   (thread (or (as v) (gen 1)) v)))
   
