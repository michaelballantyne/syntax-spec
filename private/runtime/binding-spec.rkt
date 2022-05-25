#lang racket/base

(provide
 (struct-out ref)      ; v:binding-class
 (struct-out subexp)   ; v:nonterminal
 (struct-out bind)     ; !
 (struct-out scope)    ; {}
 (struct-out group)    ; []
 (struct-out nest)
 (struct-out nest-one)
 (struct-out nested)
 (struct-out suspend)

 qualifier?
 svar?
 nonterm?

 binding-spec-well-formed?

 simple-expand
 expand-function-return
 simple-expand-single-exp

 do-bind!)

(require
  racket/match
  racket/list
  racket/set
  racket/syntax
  racket/pretty
  ee-lib
  (for-template
   "compile.rkt")
  "../syntax/syntax-classes.rkt")

;;
;; Representation
;;

;; Binding `spec`
;; is one of:
(struct ref [svar space pred msg] #:transparent)
(struct subexp [svar nonterm] #:transparent)
(struct bind [svar space bvalc] #:transparent)
(struct scope [spec] #:transparent)
(struct group [specs] #:transparent)
(struct nest [svar nonterm spec] #:transparent)
(struct nest-one [svar nonterm spec] #:transparent)
(struct nested [] #:transparent)
(struct suspend [svar] #:transparent)

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
    [(ref (? svar? pv) (or #f (? symbol?)) (? procedure?) (? string?))
     (set-member? svars pv)]
    [(subexp (? svar? pv) (? nonterm?))
     (set-member? svars pv)]
    [(bind (? svar? pv) (or #f (? symbol?)) (? procedure?))
     (set-member? svars pv)]
    [(scope spec)
     (binding-spec-well-formed? spec svars)]
    [(group specs)
     (for/and ([spec specs])
       (binding-spec-well-formed? spec svars))]
    [(or (nest (? svar? pv) (? procedure? nonterm) spec)
         (nest-one (? svar? pv) (? procedure? nonterm) spec))
     (and
      (set-member? svars pv)
      (binding-spec-well-formed? spec svars))]
    [(nested)
     #t]
    [(suspend (? svar? pv))
     (set-member? svars pv)]))

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

(define (flip-intro-scope/env env)
  (for/hash ([(k v) env])
    (values k
            (for/tree ([el v])
              (flip-intro-scope el)))))

(struct exp-f-ret (spec pvar-vals reconstruct-f stx-ctx))

(define (call-expand-function f stx)
  ;; Original equivalent
  ;(flip-intro-scope (f (flip-intro-scope stx)))

  (match (f (flip-intro-scope stx))
    [(? syntax? stx^)
     (flip-intro-scope stx^)]
    [(exp-f-ret spec pvar-vals/pos reconstruct-f stx-ctx)
     (define st^
       (parameterize ([current-syntax-context stx-ctx])
         (simple-expand-internal spec (exp-state pvar-vals/pos #f) '())))
     (call-reconstruct-function (exp-state-pvar-vals st^) reconstruct-f)]))

(define (call-reconstruct-function pvar-vals reconstruct-f)
  (flip-intro-scope (reconstruct-f (flip-intro-scope/env pvar-vals))))

(define (expand-function-return spec pvar-vals reconstruct-f)
  (exp-f-ret spec (flip-intro-scope/env pvar-vals) reconstruct-f (current-syntax-context)))


;; Use only for the initial call at an interface macro.
;; spec, env -> env
(define (simple-expand spec pvar-vals)
  (parameterize ([current-orig-stx (current-syntax-context)])
    (define posspace-env (flip-intro-scope/env pvar-vals))
    (define res (simple-expand-internal spec (exp-state posspace-env #f) '()))
    (flip-intro-scope/env (exp-state-pvar-vals res))))

(define (simple-expand-single-exp f stx)
  (hash-ref (simple-expand (subexp 'inject f) (hash 'inject stx))
            'inject))

; Note: expects negative-space id, just like ee-lib `bind!`
(define do-bind!
  (make-parameter
   (lambda (id val #:space [space #f])
     (error 'do-bind! "internal error: not in a context where do-bind! is defined"))))

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
    
    [(ref pv space pred msg)
     (for/pv-state-tree ([id pv])
       (define id^ (add-scopes id local-scopes))
       (when (not (lookup (flip-intro-scope id^) pred #:space space))
         ;(pretty-write (syntax-debug-info (flip-intro-scope id^) 0 #t))
         (wrong-syntax (flip-intro-scope id^) msg))
       id^)]
    
    [(subexp pv f)
     (for/pv-state-tree ([stx pv])
       (call-expand-function f (add-scopes stx local-scopes)))]
    
    [(bind pv space constr-id)
     (for/pv-state-tree ([stx pv])
       (flip-intro-scope
        ((do-bind!) (flip-intro-scope (add-scopes stx local-scopes)) #`(#,constr-id) #:space space)))]
    
    [(scope spec)
     (with-scope sc
       (parameterize ([do-bind! bind!])
         (simple-expand-internal spec st (cons sc local-scopes))))]
    
    [(group specs)     
     (for/fold ([st st])
               ([spec specs])
       (simple-expand-internal spec st local-scopes))]
    
    [(nest pv f inner-spec)
     (define init-seq (get-pvar st pv))

     (define res
       (simple-expand-nest (nest-call f init-seq '() st inner-spec) local-scopes))
     
     (match-define (nest-ret done-seq st^) res)
     
     (set-pvar st^ pv done-seq)]

    [(nest-one pv f inner-spec)
     (define init-seq (list (get-pvar st pv)))

     (define res
       (simple-expand-nest (nest-call f init-seq '() st inner-spec) local-scopes))
     
     (match-define (nest-ret done-seq st^) res)
     
     (set-pvar st^ pv (car done-seq))]
    
    [(nested)
     (update-nest-state
      st
      (lambda (nest-st)
        (simple-expand-nest nest-st local-scopes)))]

    [(suspend pv)
     (for/pv-state-tree ([stx pv])
       (datum->syntax #f
                      (suspension stx
                                  (current-def-ctx))))]))

; f is nonterm-transformer
; seq is (listof (treeof syntax?))
; inner-spec-st is exp-state?
; inner-spec is spec
(struct nest-call [f seq acc-scopes inner-spec-st inner-spec] #:transparent)

; seq is (listof (treeof syntax?))
(struct nest-ret [done-seq inner-spec-st^] #:transparent)

(define (display-scopes l)
  (pretty-write (syntax-debug-info (add-scopes (datum->syntax #f '||) l))))

(define (call-expand-function/nest f stx nest-st)
  (match (f (flip-intro-scope stx))
    [(? syntax? stx^)
     (values (flip-intro-scope stx^) nest-st)]
    [(exp-f-ret spec pvar-vals/pos reconstruct-f stx-ctx)
     (define st^
       (parameterize ([current-syntax-context stx-ctx])
         (simple-expand-internal spec (exp-state pvar-vals/pos nest-st) '())))
     (values
      (call-reconstruct-function (exp-state-pvar-vals st^) reconstruct-f)
      (exp-state-nest-state st^))]))

; nest-call? -> nest-ret?
(define (simple-expand-nest nest-st new-local-scopes)
  (match-define (nest-call f seq acc-scopes inner-spec-st inner-spec) nest-st)
  
  (define acc-scopes^ (append acc-scopes new-local-scopes))

  (match seq
    [(cons stx rest)
     (define-values
       (stx^ nest-st^)
       ;; Original:
       (call-expand-function/nest
        f
        (add-scopes stx acc-scopes^)
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

