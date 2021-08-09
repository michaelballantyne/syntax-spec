#lang racket/base

(require racket/match racket/set)

; Binding spec
; one of:
(struct subexp [pvar nonterm])
(struct bind [pvar])
(struct export [pvars qualifier])
(struct scope [specs])
(struct group [specs])

; Export qualifier
; one of:
;   'disjoint
;   'same
;   'union

(define (qualifier? v)
  (member v '(disjoint same union)))

(define (pvar? v)
  (symbol? v))

(define (binding-spec-well-formed? spec pvars)
  (match spec
    [(subexp (? pvar? pv) (? procedure?))
     (set-member? pvars pv)]
    [(bind (? pvar? pv))
     (set-member? pvars pv)]
    [(export (list-rest (? pvar? pvs)) (? qualifier?))
     (for/and ([pv pvs])
       (set-member? pvars pv))]
    [(scope specs)
     (for/and ([spec specs])
       (binding-spec-well-formed? spec pvars))]
    [(group specs)
     (for/and ([spec specs])
       (binding-spec-well-formed? spec pvars))]))
