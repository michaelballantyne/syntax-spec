#lang racket/base

(provide
 simple-expand)

(require
  racket/match
  ee-lib
  "spec.rkt")

;; `exp-state` is (hashof symbol? syntax?)

;; spec, exp-state -> exp-state
(define (simple-expand spec exp-state)
  (simple-expand-internal spec exp-state '()))

;; spec, exp-state, (listof scope-tagger) -> exp-state
(define (simple-expand-internal spec exp-state local-scopes)
  (match spec
    [(ref (? svar? pv) (? procedure? pred) (? string? msg))
     (define id (hash-ref exp-state pv))
     (when (not (lookup id pred))
       (raise-syntax-error #f msg id))
     exp-state]
    [(subexp (? svar? pv) (? nonterm? f))
     (hash-update exp-state pv
                  (lambda (stx)
                    (f (add-scopes stx local-scopes))))]
    [(bind (? svar? pv) (? procedure? valc))
     (hash-update exp-state pv
                  (lambda (stx)
                    (bind! (add-scopes stx local-scopes) (valc))))]
    [(export (list-rest (? svar? pvs)) (? qualifier?))
     (error 'simple-expand "export specs not supported")]
    [(scope spec)
     (with-scope sc
       (simple-expand-internal spec exp-state (cons sc local-scopes)))]
    [(group specs)
     (for/fold ([exp-state exp-state])
               ([spec specs])
       (simple-expand-internal spec exp-state local-scopes))]))

