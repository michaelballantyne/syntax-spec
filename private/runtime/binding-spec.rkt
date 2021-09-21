#lang racket/base

(provide
 (struct-out ref)      ; v:binding-class
 (struct-out subexp)   ; v:nonterminal
 (struct-out bind)     ; !
 (struct-out export)   ; ^
 (struct-out scope)    ; {}
 (struct-out group)    ; []
 (struct-out nest) ; fold
 (struct-out nested) ; tail

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
(struct nest [svar nonterm spec] #:transparent)
(struct nested [] #:transparent)

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

;; `nonterm-transformer` is (syntax?, maybe-nest-state?) -> (syntax?, maybe-nest-state?)
;;
;; Note: right now nonterminal expander must *not* be hygienic
;;  in the sense of define/hygienic; otherwise continuing a `nest`
;;  with `nested` will have the wrong scopes.
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
    [(nest (? svar? pv) (? procedure? nonterm) spec)
     (and
      (set-member? svars pv)
      (binding-spec-well-formed? spec svars))]
    [(nested)
     #t]))

;;
;; Expansion
;;

;; pvar-vals is (hashof symbol? (treeof syntax?))
;; nest-state is #f, nest-call, or nest-ret
(struct exp-state [pvar-vals nest-state])

;; Helpers for accessing and updating parts of the exp-state

(define (get-pvar st pv)
  (hash-ref (exp-state-pvar-vals st) pv))

(define (set-pvar st pv val)
  (struct-copy
   exp-state st
   [pvar-vals (hash-set (exp-state-pvar-vals st) pv val)]))

(define (update-pvar st pv f)
  (struct-copy
   exp-state st
   [pvar-vals (hash-update
               (exp-state-pvar-vals st) pv f)]))

(define (update-nest-state st f)
  (struct-copy
   exp-state st
   [nest-state (f (exp-state-nest-state st))]))


;; spec, env, (or/c #f nest-call?) -> env, (or/c #f nest-ret?)
(define (simple-expand spec pvar-vals nest-st)
  (define res (simple-expand-internal spec (exp-state pvar-vals nest-st) '()))
  (values
   (exp-state-pvar-vals res)
   (exp-state-nest-state res)))

;; spec, exp-state, (listof scope-tagger) -> exp-state
(define (simple-expand-internal spec st local-scopes)
  
  (define-syntax-rule
    ;; update the state of `pv` by mapping over its tree
    (for/pv-state-tree ([item pv])
      b ...)
    (update-pvar st pv
                 (lambda (st)
                   (for/tree ([item st])
                     b ...))))
                  
  (match spec
    
    [(ref pv pred msg)
     (for/pv-state-tree ([id pv])
       (define id^ (add-scopes id local-scopes))
       (when (not (lookup id^ pred))
         (raise-syntax-error #f msg id^))
       id^)]
    
    [(subexp pv f)
     (for/pv-state-tree ([stx pv])
       (let-values ([(res _) (f (add-scopes stx local-scopes) #f)])
         res))]
    
    [(bind pv valc)
     (for/pv-state-tree ([stx pv])
       (bind! (add-scopes stx local-scopes) (valc)))]
    
    [(export (list-rest (? svar? pvs)) (? qualifier?))
     (error 'simple-expand "export specs not supported")]
    
    [(scope spec)
     (with-scope sc
       (simple-expand-internal spec st (cons sc local-scopes)))]
    
    [(group specs)
     (define-values (binds others) (order-group specs))
     
     (for/fold ([st st])
               ([spec (append binds others)])
       (simple-expand-internal spec st local-scopes))]
    
    [(nest pv f inner-spec)
     (define init-seq (for/list ([el (get-pvar st pv)])
                        (add-scopes el local-scopes)))

     (define res
       (simple-expand-nest (nest-call f init-seq '() st inner-spec) '()))
     
     (match-define (nest-ret done-seq st^) res)
     
     (set-pvar st^ pv done-seq)]
    
    [(nested)
     (update-nest-state
      st
      (lambda (nest-st)
        (simple-expand-nest nest-st local-scopes)))]))

(define (order-group specs)
  (define binds '())
  (define others '())

  (let recur ([specs specs])
    (for/list ([spec specs])
      (match spec
        [(bind _ _)
         (set! binds (cons spec binds))]
        [(group specs)
         (recur specs)]
        [_
         (set! others (cons spec others))])))

  (values (reverse binds) (reverse others)))

; f is nonterm-transformer
; seq is (listof (treeof syntax?))
; inner-spec-st is exp-state?
; inner-spec is spec
(struct nest-call [f seq acc-scopes inner-spec-st inner-spec] #:transparent)

; seq is (listof (treeof syntax?))
(struct nest-ret [done-seq inner-spec-st^] #:transparent)

; nest-call? -> nest-ret?
(define (simple-expand-nest nest-st new-local-scopes)
  (match-define (nest-call f seq acc-scopes inner-spec-st inner-spec) nest-st)
  
  (define acc-scopes^ (append acc-scopes new-local-scopes))

  (match seq
    [(cons stx rest)
     (define-values
       (stx^ nest-st^)
       (f (add-scopes stx acc-scopes^)
          (nest-call f rest acc-scopes^ inner-spec-st inner-spec)))

     (match-define (nest-ret done-seq inner-spec-st^) nest-st^)
     (nest-ret (cons stx^ done-seq) inner-spec-st^)]
    ['()
     (nest-ret '() (simple-expand-internal inner-spec inner-spec-st acc-scopes^))]))

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

