#lang racket/base

(require racket/pretty)

(provide
 (struct-out rename-ref)
 (struct-out ref)      ; v:binding-class
 (struct-out subexp/no-scope)
 (struct-out subexp)   ; v:nonterminal
 (struct-out rename-bind)
 (struct-out bind)
 (struct-out bind-syntax)
 (struct-out scope)    ; {}
 (struct-out group)    ; []
 (struct-out nest)
 (struct-out nest-one)
 (struct-out nested)
 (struct-out suspend)

 expand-top
 expand-function-return
 simple-expand-single-exp

 wrap-bind-trampoline)

(require
  racket/function
  racket/match
  racket/syntax
  racket/pretty
  ee-lib
  ee-lib/private/lift-trampoline
  (for-template
   racket/base
   "compile.rkt")
  (for-syntax
   racket/base
   syntax/parse)
  "../syntax/syntax-classes.rkt")

(module+ test (require rackunit))

(define DEBUG-RENAME #f)

;;
;; Representation
;;

;; Binding `spec`
;; is one of:
(struct rename-ref [pvar space] #:transparent)
(struct ref [pvar space pred msg] #:transparent)
(struct subexp [pvar nonterm] #:transparent)
(struct subexp/no-scope [pvar nonterm] #:transparent)
(struct rename-bind [pvar space] #:transparent)
(struct bind [pvar space bvalc] #:transparent)
(struct bind-syntax [pvar space bvalc transformer-pvar] #:transparent)
(struct scope [spec] #:transparent)
(struct group [specs] #:transparent)
(struct nest [pvar nonterm spec] #:transparent)
(struct nest-one [pvar nonterm spec] #:transparent)
(struct nested [] #:transparent)
(struct suspend [pvar] #:transparent)

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

(define (update-pvar* st pvs f)
  (define env (exp-state-pvar-vals st))
  (define vals (for/list ([pv pvs]) (hash-ref env pv)))
  (define vals^ (call-with-values (lambda () (apply f vals))
                     list))
  (define env^
    (for/fold ([env^ env])
              ([pv pvs]
               [val^ vals^])
      (hash-set env^ pv val^)))
  (struct-copy
   exp-state st
   [pvar-vals env^]))

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

(define (expand-function-return spec-list pvar-vals reconstruct-f)
  ;; accepts a list for consistency with expand-top, but only a single spec is ever provided
  (match-define (list spec) spec-list)
  (exp-f-ret spec (flip-intro-scope/env pvar-vals) reconstruct-f (current-syntax-context)))


;; Use only for the initial call at an interface macro.
;; spec, env -> env
(define (expand-top pass-specs pvar-vals k)
  (parameterize ([current-orig-stx (current-syntax-context)])
    (define posspace-env (flip-intro-scope/env pvar-vals))
    (define res
      (for/fold ([env posspace-env])
                ([pass-spec pass-specs])
        (exp-state-pvar-vals (simple-expand-internal pass-spec (exp-state env #f) '()))))
    (k (flip-intro-scope/env res))))

(define (simple-expand-single-exp f stx)
  (expand-top (list (subexp 'inject f)) (hash 'inject stx) (lambda (env^) (hash-ref env^ 'inject))))

; Note: expects negative-space id, just like ee-lib `bind!`
(define do-bind!
  (make-parameter
   (lambda (id val #:space [space #f])
     (error 'do-bind! "internal error: not in a context where do-bind! is defined"))))

;; spec, exp-state, (listof scope-tagger) -> exp-state
(define (simple-expand-internal spec st local-scopes)
  
  (define-syntax for/pv-state-tree
    ;; update the state of `pv` by mapping over its tree
    (syntax-parser
      [(_ ([item pv] ...+)
         b ...)
       #:with (tree ...) (generate-temporaries (attribute item))
       #'(update-pvar* st (list pv ...)
                       (lambda (tree ...)
                         (for/tree ([item tree] ...)
                           b ...)))]))
                  
  (match spec
    
    [(ref pv space pred msg)
     (for/pv-state-tree ([id pv])
       (when DEBUG-RENAME
         (displayln 'ref)
         (pretty-write id))
       (define id^ (add-scopes id local-scopes))
       (when (not (lookup (flip-intro-scope id^) pred #:space space))
         ;(pretty-write (syntax-debug-info (flip-intro-scope id^) 0 #t))
         (wrong-syntax (flip-intro-scope id^) msg))
       id^)]
    
    [(bind pv space constr-id)
     (for/pv-state-tree ([id pv])
       (when DEBUG-RENAME
         (displayln 'bind)
         (pretty-write (syntax-debug-info id)))
       (let ([bound-id ((do-bind!) (flip-intro-scope (add-scopes id local-scopes)) #`(#,constr-id) #:space space)])
         (compile-binder! ((in-space space) bound-id))
         (flip-intro-scope bound-id)))]

    [(bind-syntax pv space constr-id transformer-pv)
     (for/pv-state-tree ([id pv] [transformer-stx transformer-pv])
       (when DEBUG-RENAME
         (displayln 'bind-syntax)
         (pretty-write (syntax-debug-info id)))
       (define scoped-transformer-stx (add-scopes transformer-stx local-scopes))
       (let ([bound-id ((do-bind!) (flip-intro-scope (add-scopes id local-scopes)) #`(#,constr-id #,(flip-intro-scope scoped-transformer-stx)) #:space space)])
         (compile-binder! ((in-space space) bound-id))
         (values (flip-intro-scope bound-id) scoped-transformer-stx)))]
    [(rename-ref pv space)
     (for/pv-state-tree ([id pv])
       (when DEBUG-RENAME
         (displayln 'rename-ref/spaced)
         (pretty-write (syntax-debug-info ((in-space space) id))))
       (flip-intro-scope (compile-reference (flip-intro-scope ((in-space space) id)))))]

    [(rename-bind pv space)
     (for/pv-state-tree ([id pv])
       (when DEBUG-RENAME
         (displayln 'rename-bind/spaced)
         (pretty-write (syntax-debug-info ((in-space space) id))))
       (flip-intro-scope (compile-binder! (flip-intro-scope ((in-space space) id)) #:reuse? #t)))]

    [(subexp/no-scope pv f)
     (for/pv-state-tree ([stx pv])
       (call-expand-function f stx))]
    
    [(subexp pv f)
     (for/pv-state-tree ([stx pv])
       (call-expand-function f (add-scopes stx local-scopes)))]
    
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
       (start-nest f init-seq st inner-spec local-scopes))
     
     (match-define (nest-ret done-seq st^) res)
     
     (set-pvar st^ pv done-seq)]

    [(nest-one pv f inner-spec)
     (define init-seq (list (get-pvar st pv)))

     (define res
       (start-nest f init-seq st inner-spec local-scopes))
     
     (match-define (nest-ret done-seq st^) res)
     
     (set-pvar st^ pv (car done-seq))]
    
    [(nested)
     (update-nest-state
      st
      (lambda (nest-st)
        (simple-expand-nest nest-st local-scopes)))]

    [(suspend pv)
     (for/pv-state-tree ([stx pv])
       (make-suspension (add-scopes stx local-scopes) (current-def-ctx)))]))

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

;; When entering a `nest-one` or `nest` form, add an extra scope. This means that the
;; expansion within is in a new definition context with a scope distinguishing it from
;; surrounding definition contexts where macros may have been defined. The Racket expander
;; knows it can omit use-site scopes in this situation. This avoids a problem with situations
;; like:
;;
;; (define-syntax m (syntax-rules ()
;;                    [(m a) a]))
;; (match [(m x)
;;         ; => expands to
;;         x                               ; use-site l
;;         x])                             ; l
;;
;; where a binding created in the nest spec is not visible in the nested spec.
;;
;; TODO: if the meta-DSL supports binding macros in the future, we may need to add a scope
;; at every nest step rather than only the entry point to account for macros defined in one
;; nest step and used in the next without an intervening new scope.
(define (start-nest f init-seq st inner-spec local-scopes)
  (with-scope sc
    (simple-expand-nest (nest-call f init-seq '() st inner-spec) (cons sc local-scopes))))

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
(define-syntax for/tree
  (syntax-parser
    [(_ ([item init] ...+) body ...)
     #'(apply values (map/trees (lambda (item ...) body ...) (list init ...)))]))

; trees must have same structure and f must always return the same number of values given a particular
; number of arguments.
; traverse the trees in parallel, applying f at each leaf with arguments from each tree.
; Constructs as many parallel trees as return values, each with the same shape as the input trees.
(define (map/trees f trees)
  (cond
    [(andmap (negate list?) trees) (call-with-values (lambda () (apply f trees)) list)]
    [(and (andmap list? trees) (apply = (map length trees)))
     (transpose (apply map (lambda items (map/trees f items)) trees))]
    ; TODO make this a better error that reports in terms of binding specs
    [else (error 'map/tree "tree shapes are not the same")]))

(module+ test
  (check-equal? (map/trees list '())
                '(()))
  (check-equal? (map/trees (lambda (v1 v2) (values (add1 v1) (sub1 v2)))
                           '(1 2))
                '(2 1))
  (check-equal? (map/trees (lambda (v1 v2) (values (add1 v1) (sub1 v2)))
                           '((1 2 3) (3 4 5)))
                '((2 3 4) (2 3 4)))
  (check-equal? (map/trees (lambda (v1 v2) (values (add1 v1) (vector v1 v2)))
                           '((1 (2) 3) (3 (4) 5)))
                '((2 (3) 4) (#(1 3) (#(2 4)) #(3 5))))
  (check-equal? (map/trees add1 '((((2) (3 4)))))
                '((((3) (4 5)))))
  (check-equal? (map/trees *
                          '((((2) (3 4)))
                            (((5) (6 7)))))
                '((((10) (18 28))))))

(define (transpose v)
  (apply map list v))

(define (trampoline-bind! id rhs-e #:space [space #f])
  (define pos-id (flip-intro-scope id))
  (trampoline-lift! #`(define-syntax #,((in-space space) id) #,rhs-e))
  (flip-intro-scope pos-id))

(define (wrap-bind-trampoline transformer)
  (wrap-lift-trampoline
   (lambda (stx)
     (parameterize ([do-bind! trampoline-bind!])
       (transformer stx)))))
