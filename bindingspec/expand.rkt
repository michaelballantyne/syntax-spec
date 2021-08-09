#lang racket/base

(provide
 simple-expand)

(require
  racket/match
  ee-lib
  "spec.rkt")

; `exp-state` is (hashof symbol? syntax?)

; spec, exp-state, (listof scope?) -> exp-state
(define (simple-expand spec exp-state local-scopes)
  (match spec
    [(ref (? pvar? pv) (? procedure? pred) (? string? msg))
     (define id (hash-ref exp-state pv))
     (when (not (lookup id pred))
       (raise-syntax-error #f msg id))
     exp-state]
    [(subexp (? pvar? pv) (? nonterm? f))
     (hash-update exp-state pv
                  (lambda (stx)
                    (f (add-scopes stx local-scopes))))]
    [(bind (? pvar? pv) (? procedure? valc))
     (hash-update exp-state pv
                  (lambda (stx)
                    (bind! (add-scopes stx local-scopes) (valc))))]
    [(export (list-rest (? pvar? pvs)) (? qualifier?))
     (error 'simple-expand "export specs not supported")]
    [(scope spec)
     (with-scope sc
       (simple-expand spec exp-state (cons sc local-scopes)))]
    [(group specs)
     (for/fold ([exp-state exp-state])
               ([spec specs])
       (simple-expand spec exp-state local-scopes))]))

