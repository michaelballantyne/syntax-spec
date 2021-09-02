#lang racket/base

(provide
 (struct-out ref)     ; v:binding-class
 (struct-out subexp)  ; v:nonterminal
 (struct-out bind)    ; !
 (struct-out export)  ; ^
 (struct-out scope)   ; {}
 (struct-out group)   ; []

 qualifier?
 svar?
 nonterm?

 binding-spec-well-formed?

 simple-expand)
  

(require
  racket/match
  racket/list
  racket/set
  ee-lib)

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








