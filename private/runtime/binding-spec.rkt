#lang racket/base

(provide
 (struct-out ref)      ; v:binding-class
 (struct-out subexp)   ; v:nonterminal
 (struct-out bind)     ; !
 (struct-out export)   ; ^
 (struct-out scope)    ; {}
 (struct-out group)    ; []
 (struct-out seq-fold) ; fold
 (struct-out continue) ; tail

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


;;
;; Representation
;;

;; Binding `spec`
;; is one of:
(struct ref [svar pred msg] #:transparent)
(struct subexp [svar nonterm] #:transparent)
(struct bind [svar bvalc] #:transparent)
(struct export [svars qualifier] #:transparent)
(struct scope [spec] #:transparent)
(struct group [specs] #:transparent)
(struct seq-fold [svar nonterm spec] #:transparent)
(struct continue [k] #:transparent)

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
       (binding-spec-well-formed? spec svars))]
    [(seq-fold (? svar? pv) (? procedure? nonterm) spec)
     (and
      (set-member? svars pv)
      (binding-spec-well-formed? spec svars))]
    [(continue (? procedure? k))
     #t]))


;;
;; Expansion
;;

;; `exp-state` is (hashof symbol? (treeof syntax?))

;; spec, exp-state -> exp-state
(define (simple-expand spec exp-state)
  (simple-expand-internal spec exp-state '()))

;; spec, exp-state, (listof scope-tagger) -> exp-state
(define (simple-expand-internal spec exp-state local-scopes)
  (match spec
    [(ref pv pred msg)
     (hash-update exp-state pv
                  (lambda (ids)
                    (for/tree ([id ids])
                      (define id^ (add-scopes id local-scopes))
                      (when (not (lookup id^ pred))
                        (raise-syntax-error #f msg id^))
                      id^)))]
    [(subexp pv f)
     (hash-update exp-state pv
                  (lambda (stxs)
                    (for/tree ([stx stxs])
                      (f (add-scopes stx local-scopes)))))]
    [(bind pv valc)
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
       (simple-expand-internal spec exp-state local-scopes))]
    [(seq-fold pv f tail-spec)
     (define init-seq (for/list ([el (hash-ref exp-state pv)])
                        (add-scopes el local-scopes)))
     
     (define tail-result (box #f))
     (define result-list (box '()))
     
     (define (finish! tail-exp-state)
       (set-box! tail-result tail-exp-state))

     (define (add-result! result)
       (set-box! result-list (cons result (unbox result-list))))
     
     (let loop ([seq init-seq]
                [acc-scopes '()])
       (define (k caller-local-scopes)
         (loop (for/list ([el (cdr seq)])
                 (add-scopes el caller-local-scopes))
               (append acc-scopes caller-local-scopes)))
       (if (null? seq)
           (finish!
            (simple-expand-internal
             tail-spec exp-state (append local-scopes acc-scopes)))
           (add-result!
            (f (car seq) k))))
     
     (hash-set (unbox tail-result) pv (unbox result-list))]
    
    [(continue k)
     (begin
       (k local-scopes)
       exp-state)]))

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
          (let () body ...)))))








