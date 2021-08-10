#lang racket/base

(provide
 (struct-out ref)     ; ~
 (struct-out subexp)  ; :
 (struct-out bind)    ; !
 (struct-out export)  ; ^
 (struct-out scope)   ; {}
 (struct-out group)   ; []

 qualifier?
 svar?
 nonterm?

 binding-spec-well-formed?)
  

(require
  racket/match
  racket/set)

;; Binding `spec`
;; is one of:
(struct ref [svar pred msg])
(struct subexp [svar nonterm])
(struct bind [svar bvalc])
(struct export [svars qualifier])
(struct scope [spec])
(struct group [specs])

;; `bvalc` is (-> any/c)

;; Export `qualifier`
;; is one of:
;;   'disjoint
;;   'same
;;   'union
(define (qualifier? v)
  (member v '(disjoint same union)))

;; `svar` is a symbol
(define (svar? v)
  (symbol? v))

;; `nonterm` is syntax? -> syntax?
;; The system assumes this procedure is defined with define/hygienic
(define (nonterm? v)
  (procedure? v))

;; spec, (setof svars) -> (or/c #f any/c)
(define (binding-spec-well-formed? spec svars)
  (match spec
    [(ref (? svar? pv) (? procedure?) (? string?))
     (set-member? svars pv)]
    [(subexp (? svar? pv) (? nonterm?))
     (set-member? svars pv)]
    [(bind (? svar? pv) (? procedure?))
     (set-member? svars pv)]
    [(export (list-rest (? svar? pvs)) (? qualifier?))
     (for/and ([pv pvs])
       (set-member? svars pv))]
    [(scope spec)
     (binding-spec-well-formed? spec svars)]
    [(group specs)
     (for/and ([spec specs])
       (binding-spec-well-formed? spec svars))]))