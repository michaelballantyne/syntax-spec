#lang racket/base

(provide
 (struct-out ref)
 (struct-out subexp)
 (struct-out bind)
 (struct-out export)
 (struct-out scope)
 (struct-out group)

 qualifier?
 pvar?
 nonterm?

 binding-spec-well-formed?)
  

(require
  racket/match
  racket/set)

; Binding `spec`
; is one of:
(struct ref [pvar pred msg])
(struct subexp [pvar nonterm])
(struct bind [pvar bvalc])
(struct export [pvars qualifier])
(struct scope [spec])
(struct group [specs])

; `bvalc` is (-> any/c)

; Export `qualifier`
; is one of:
;   'disjoint
;   'same
;   'union
(define (qualifier? v)
  (member v '(disjoint same union)))

; `pvar` is a symbol
(define (pvar? v)
  (symbol? v))

; `nonterm` is syntax? -> syntax?
; The system assumes this procedure is defined with define/hygienic
(define (nonterm? v)
  (procedure? v))

; spec, (setof pvars) -> (or/c #f any/c)
(define (binding-spec-well-formed? spec pvars)
  (match spec
    [(ref (? pvar? pv) (? procedure?) (? string?))
     (set-member? pvars pv)]
    [(subexp (? pvar? pv) (? nonterm?))
     (set-member? pvars pv)]
    [(bind (? pvar? pv) (? procedure?))
     (set-member? pvars pv)]
    [(export (list-rest (? pvar? pvs)) (? qualifier?))
     (for/and ([pv pvs])
       (set-member? pvars pv))]
    [(scope spec)
     (binding-spec-well-formed? spec pvars)]
    [(group specs)
     (for/and ([spec specs])
       (binding-spec-well-formed? spec pvars))]))