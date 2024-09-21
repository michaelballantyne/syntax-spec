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
 (struct-out bind-syntaxes)
 (struct-out scope)
 (struct-out group)    ; []
 (struct-out nest)
 (struct-out nest-one)
 (struct-out nested)
 (struct-out suspend)
 (struct-out fresh-env-expr-ctx)
 (struct-out ellipsis) ; ...

 ; used by interface macros
 expand-top
 ; used in clauses of nonterminal expanders
 expand-function-return
 ; used to generate a local-expand function for some nonterminal
 make-local-expand-entry-point

 wrap-bind-trampoline)

(require
  racket/function
  racket/match
  racket/syntax
  racket/pretty
  "../ee-lib/main.rkt"
  "../ee-lib/lift-trampoline.rkt"
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
(struct bind-syntaxes [depth pvar space bvalc transformer-pvar] #:transparent)
(struct scope [spec] #:transparent)
(struct group [specs] #:transparent)
(struct nest [pvar nonterm spec] #:transparent)
(struct nest-one [pvar nonterm spec] #:transparent)
(struct nested [] #:transparent)
(struct suspend [pvar] #:transparent)
(struct fresh-env-expr-ctx [spec] #:transparent)
(struct ellipsis [pvars spec] #:transparent)

;;
;; Expansion
;;

; A NestState is one of
;; #f, nest-call, or nest-ret

;; pvar-vals is (hashof symbol? (treeof syntax?))
(struct exp-state [pvar-vals nest-state])

;; Helpers for accessing and updating parts of the exp-state

; exp-state? symbol? -> (treeof syntax?)
(define (get-pvar st pv)
  (hash-ref (exp-state-pvar-vals st) pv))

; exp-state? symbol? (treeof syntax?) -> exp-state?
(define (set-pvar st pv val)
  (struct-copy
   exp-state st
   [pvar-vals (hash-set (exp-state-pvar-vals st) pv val)]))

; exp-state? (listof symbol?) ((treeof syntax?) ... -> (treeof syntax?)) -> exp-state?
; updates the environment by applying f to the values of pvs
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

; exp-state? (NestState -> NestState) -> exp-state?
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

#;(-> (-> syntax? any?) (->* (syntax?) (#:should-rename? boolean?) any?))
; apply the procedure with renaming disabled
(define ((make-local-expand-entry-point f) stx #:should-rename? [should-rename?/val #f])
  (parameterize ([should-rename? should-rename?/val])
    (expand-top (list (subexp 'inject f)) (hash 'inject stx) (lambda (env^) (hash-ref env^ 'inject)))))

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

; Note: expects negative-space id, just like ee-lib `bind!`
(define do-bind!
  (make-parameter
   (lambda (id val #:space [space #f])
     (error 'do-bind! "internal error: not in a context where do-bind! is defined"))))

; expects and returns a positive space id (or list of ids)
(define (bind-and-record-rename! ids val space)
  (if (identifier? ids)
      (car (bind-and-record-rename! (list ids) val space))
      (let ([bound-ids ((do-bind!) (for/list ([id ids]) (flip-intro-scope id)) val #:space space)])
        (when (should-rename?) (for ([bound-id bound-ids]) (compile-binder! ((in-space space) bound-id))))
        (for/list ([bound-id bound-ids]) (flip-intro-scope bound-id)))))

(define should-rename? (make-parameter #t))

(define (map-values proc thnk)
  (let ([lst (call-with-values thnk list)])
    (apply values (map proc lst))))

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
    [(fresh-env-expr-ctx spec)
     (syntax-local-apply-transformer
      (lambda ()
        (with-scope sc
          (simple-expand-internal spec st (cons sc local-scopes))))
      #f
      'expression
      #f)]
   
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
       (bind-and-record-rename! (add-scopes id local-scopes) #`(#,constr-id) space))]

    [(bind-syntax pv space constr-id transformer-pv)
     (for/pv-state-tree ([transformer-stx transformer-pv] [id pv])
       (when DEBUG-RENAME
         (displayln 'bind-syntax)
         (pretty-write (syntax-debug-info id)))
       (unless (identifier? id)
         (error 'bind-syntax "ellipsis depth mismatch"))
       (define scoped-transformer-stx (add-scopes transformer-stx local-scopes))
       (let ([bound-id (bind-and-record-rename! (add-scopes id local-scopes) #`(#,constr-id #,(flip-intro-scope scoped-transformer-stx)) space)])
         (values scoped-transformer-stx bound-id)))]
    [(bind-syntaxes depth pv space constr-id transformer-pv)
     (unless (= 1 depth)
       (error "don't know how to handle depth > 1 yet"))
     (for/pv-state-tree ([transformer-stx transformer-pv] [ids pv])
       (when DEBUG-RENAME
         (displayln 'bind-syntaxes)
         (pretty-write (syntax-debug-info ids)))
       (unless (and (list? ids) (for/and ([id ids]) (identifier? id)))
         (error 'bind-syntaxes "ellipsis depth mismatch"))
       (define scoped-transformer-stx (add-scopes transformer-stx local-scopes))
       (let ([bound-ids (bind-and-record-rename! (for/list ([id ids]) (add-scopes id local-scopes))
                                                 #`(map-values #,constr-id (lambda () #,(flip-intro-scope scoped-transformer-stx)))
                                                 space)])
         (values scoped-transformer-stx bound-ids)))]
    [(rename-ref pv space)
     (for/pv-state-tree ([id pv])
       (when DEBUG-RENAME
         (displayln 'rename-ref/spaced)
         (pretty-write (syntax-debug-info ((in-space space) id))))
       (if (should-rename?)
           (flip-intro-scope (compile-reference (flip-intro-scope ((in-space space) id))))
           ((in-space space) id)))]
    [(rename-bind pv space)
     (for/pv-state-tree ([id pv])
       (when DEBUG-RENAME
         (displayln 'rename-bind/spaced)
         (pretty-write (syntax-debug-info ((in-space space) id))))
       (if (should-rename?)
           (flip-intro-scope (compile-binder! (flip-intro-scope ((in-space space) id)) #:reuse? #t))
           ((in-space space) id)))]

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

    ; TODO deprecate
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
       (make-suspension (add-scopes stx local-scopes) (current-def-ctx)))]
    [(ellipsis pvs spec)
     ; filter and split the environments
     (define sts (exp-state-split/ellipsis st pvs))
     ; expand on each sub-environment
     (define sts^
       (for/list ([st sts])
         (simple-expand-internal spec st local-scopes)))
     ; unsplit the sub-environments and merge that into the initial env.
     (for/fold ([st st])
               ([pv pvs])
       (set-pvar st pv (for/list ([st^ sts^]) (hash-ref (exp-state-pvar-vals st^) pv))))]))

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

; exp-state? (listof symbol?) -> (listof exp-state?)
(define (exp-state-split/ellipsis st pvars)
  (match st
    [(exp-state pvar-vals nest-state)
     (for/list ([env (env-split/ellipsis pvar-vals pvars)])
       (exp-state env nest-state))]))

; (hash symbol? (treeof syntax?)) (listof symbol?) -> (listof (hash symbol? (treeof syntax?)))
; Spit up the environment into a list of envs. One per element of a pvar's value.
; Filters environment to just pvars.
; Pvars should be mapped to lists of equal lengths. Errors if they aren't.
(define (env-split/ellipsis env pvars)
  (define env-filtered
       (for/hash ([(pv vs) (in-hash env)]
                  #:when (member pv pvars))
         (values pv vs)))
     (define repetition-length (env-repetition-length env-filtered))
     (for/list ([i (in-range repetition-length)])
       (for/hash ([(pv vs) (in-hash env-filtered)])
         (values pv (list-ref vs i)))))

(module+ test
  (check-equal? (env-split/ellipsis (hash 'a '(1 2 3) 'b '(4 5 6) 'c '())
                                          '(a b))
                (list (hash 'a 1 'b 4)
                      (hash 'a 2 'b 5)
                      (hash 'a 3 'b 6))))

; (hash symbol? (treeof syntax?)) -> natural?
; Assuming this environment is getting split for ellipses, computes how many environments it should get split into.
; Errors if not all trees have the same length.
(define (env-repetition-length env)
  (define result
    (or (for/first ([(_ vs) (in-hash env)])
          (unless (list? vs)
            ; TODO check in compiler
            (error "too many ellipses in binding spec"))
          (length vs))
        0))
  (for ([(_ vs) (in-hash env)])
    (unless (= (length vs) result)
      ; TODO Can this be checked in the compiler? Would need to make sure ellipsized bs vars
      ; come from the same ss ellipsis.
      (error "incompatible ellipsis match counts for binding spec")))
  result)

(module+ test
  (check-equal? (env-repetition-length (hash 'a '(1 2 3) 'b '(4 5 6)))
                3))

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

#;(procedure? (listof tree) -> (listof tree))
; maps a function over the leaves of the trees.
; the trees must have the same structure.
; the number of trees, the number of arguments f accepts, the number of values f returns, and the number
; of trees returned by this function must be equal.
; If this is violated, unexpected results may be returned. This is not validated.
; Traverse the trees in parallel, applying f at each leaf with arguments from each tree.
; In the case of multiple trees and multiple return values for f, applies the function
; to the corresponding leaves from all trees and returns parallel trees for each return value.
(define (map/trees f trees)
  (cond
    [(null? trees) (error 'map/trees "expected at least one tree")]
    [(not (list? (car trees))) (call-with-values (lambda () (apply f trees)) list)]
    [(andmap null? trees)
     ; this assumes that the number of output trees is the same as the number of input trees
     trees]
    [(and (andmap list? trees) (apply = (map length trees)))
     ; since we convert a leaf's resulting return values to a list, we must transpose to convert a tree of lists
     ; to a list of trees.
     (transpose (apply map (lambda items (map/trees f items)) trees))]
    ; TODO make this a better error that reports in terms of binding specs
    [else (error 'map/tree "tree shapes are not the same")]))

(module+ test
  (check-exn #rx"expected at least one tree" (lambda () (map/trees values '())))
  (check-exn #rx"tree shapes are not the same"
             (lambda () (map/trees values '((1 ((2)) 3)
                                            (4 (5) 6)))))
  (check-equal?
   (map/trees values '((1 (2) 3)
                         (4 ((5)) 6)))
   '((1 (2) 3) (4 ((5)) 6)))
  ; the function never gets applied because there are no leaves
  (check-equal? (map/trees error '(()))
                '(()))
  (check-equal? (map/trees add1 '(1)) '(2))
  (check-equal? (map/trees add1 '((1 ((2) (3 4)))))
                '((2 ((3) (4 5)))))
  (check-equal? (map/trees (lambda (v1 v2) (values (add1 v1) (- v2)))
                           '(1 2))
                '(2 -2))
  (check-equal? (map/trees (lambda (v1 v2) (values (add1 v1) (- v2)))
                           '((1 2 3) (3 4 5)))
                '((2 3 4) (-3 -4 -5)))
  (check-equal? (map/trees (lambda (v1 v2) (values (add1 v1) (- v2)))
                           '((1 (2) 3)
                             (3 (4) 5)))
                '((2 (3) 4) (-3 (-4) -5)))
  (check-equal? (map/trees (lambda (v1 v2) (values (add1 v1) (- v2)))
                           '((1 (2) (()) 3)
                             (3 (4) (()) 5)))
                '((2 (3) (()) 4) (-3 (-4) (()) -5)))
  (check-equal? (map/trees (lambda (v1 v2) (values (add1 v1) (vector v1 v2)))
                           '((1 (2) 3) (3 (4) 5)))
                '((2 (3) 4)
                  (#(1 3) (#(2 4)) #(3 5)))))

(define (transpose v)
  (apply map list v))

#;(-> (listof identifier?) syntax? #:space [(or/c #f symbol?)] (listof identifier?))
#;(-> identifier? syntax? #:space [(or/c #f symbol?)] identifier?)
; binds the identifier or identifiers as syntax to the transformer with syntax rhs-e
; by lifting a define-syntaxes
(define (trampoline-bind! id-or-ids rhs-e #:space [space #f])
  (let* ([is-list? (list? id-or-ids)]
         [ids (if is-list? id-or-ids (list id-or-ids))])
    (define pos-ids (map flip-intro-scope ids))
    (trampoline-lift! #`(define-syntaxes #,(for/list ([id ids]) ((in-space space) id))
                          #,rhs-e))
    ; flip-intro-scope is an effect that depends on the current macro-introduction scope
    ; trampoline-lift! will change the current macro-introduction scope between the first and second flips
    (let ([ids (map flip-intro-scope pos-ids)])
      (if is-list? ids (car ids)))))

(define (wrap-bind-trampoline transformer)
  (wrap-lift-trampoline
   (lambda (stx)
     (parameterize ([do-bind! trampoline-bind!])
       (transformer stx)))))
