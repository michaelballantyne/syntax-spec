#lang racket/base

(provide
 simple-expand)

(require
  racket/match
  racket/list
  ee-lib
  "spec.rkt")

;; maps over a tree
;;
;; tree := (listof tree)
;;       | any/c
(define-syntax-rule
  (for/tree ([item init]) body ...)
  (let for-nested ([list-at-depth init])
    (let ([item list-at-depth])
      (if (list? item)
          (for/list ([nested item])
            (for-nested nested))
          (begin body ...)))))

;; `exp-state` is (hashof symbol? (treeof syntax?))

;; spec, exp-state -> exp-state
(define (simple-expand spec exp-state)
  (simple-expand-internal spec exp-state '()))

;; spec, exp-state, (listof scope-tagger) -> exp-state
(define (simple-expand-internal spec exp-state local-scopes)
  (match spec
    [(ref (? svar? pv) (? procedure? pred) (? string? msg))
     (define ids (hash-ref exp-state pv))
     (for ([id (flatten ids)])
       (when (not (lookup id pred))
         (raise-syntax-error #f msg id)))
     exp-state]
    [(subexp (? svar? pv) (? nonterm? f))
     (hash-update exp-state pv
                  (lambda (stxs)
                    (for/tree ([stx stxs])
                      (f (add-scopes stx local-scopes)))))]
    [(bind (? svar? pv) (? procedure? valc))
     (hash-update exp-state pv
                  (lambda (stxs)
                    (for/tree ([stx stxs])
                      (bind! (add-scopes stx local-scopes) (valc)))))]
    [(export (list-rest (? svar? pvs)) (? qualifier?))
     (error 'simple-expand "export specs not supported")]
    [(scope spec)
     (with-scope sc
       (simple-expand-internal spec exp-state (cons sc local-scopes)))]
    [(group specs)
     (for/fold ([exp-state exp-state])
               ([spec specs])
       (simple-expand-internal spec exp-state local-scopes))]))

