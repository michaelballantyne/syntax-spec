#lang racket/base

(provide compile-bspec)

(require racket/match
         syntax/parse
         racket/syntax
         syntax/parse/class/paren-shape
         syntax/id-table
         syntax/id-set
         racket/list
         (only-in syntax/parse/private/residual-ct stxclass? has-stxclass-prop?)
         ee-lib
         "../env-reps.rkt"
         "../syntax-classes.rkt"
         (for-template racket/base
                       "../../runtime/binding-spec.rkt")
         (for-syntax
          racket/base
          syntax/parse))

(define (compile-bspec maybe-bspec bound-pvars variant)
  (define bspec-stx (or maybe-bspec #'[]))

  (define bspec-elaborated (elaborate-bspec bspec-stx))
  (define bspec-with-implicits (add-implicit-pvar-refs bspec-elaborated bound-pvars))
  (define bspec-flattened (bspec-flatten-groups bspec-with-implicits))

  (define variant-compiler
    (syntax-parse variant
      [(~or #:simple (#:nesting _))
       (lambda (spec)
         (check-order/unscoped-expression spec)
         (compile-bspec-term/single-pass spec))]
      [#:pass1
       (lambda (spec)
         (check-order/two-pass spec)
         (compile-bspec-term/pass1 spec))]
      [#:pass2
       compile-bspec-term/pass2]))

  (variant-compiler bspec-flattened))

;; Elaborated representation; variables are associated with expander-environment information

(struct pvar [id info])

(struct with-stx [stx])

(struct ref [pvar])
(struct bind with-stx [pvar])
(struct rec with-stx [pvars])
(struct re-export with-stx [pvars])
(struct export with-stx [pvar])
(struct nest with-stx [pvar spec])
(struct nest-one with-stx [pvar spec])
(struct suspend with-stx [pvar])
(struct scope with-stx [spec])
(struct group [specs])

(define-match-expander s*
  (syntax-parser
    [(_ sname:id [fname:id p] ...)
     #'(struct* sname ([fname p] ...))]))

(define (map-bspec f spec)
  (match spec
    [(nest stx pv s)
     (let ([s^ (map-bspec f s)])
       (f (nest stx pv s^)))]
    [(nest-one stx pv s)
     (let ([s^ (map-bspec f s)])
       (f (nest-one stx pv s^)))]
    [(scope stx s)
     (let ([s^ (map-bspec f s)])
       (f (scope stx s^)))]
    [(group ss)
     (let ([ss^ (map (lambda (s) (map-bspec f s)) ss)])
       (f (group ss^)))]
    [_ (f spec)]))

(define (fold-bspec f spec)
  (match spec
    [(or (s* nest [spec s])
         (s* nest-one [spec s])
         (s* scope [spec s]))
     (let ([s^ (fold-bspec f s)])
       (f spec (list s^)))]
    [(group ss)
     (let ([ss^ (map (lambda (s) (fold-bspec f s)) ss)])
       (f spec ss^))]
    [_ (f spec '())]))


;; Elaborate

(define elaborate-bspec
  (syntax-parser
    #:datum-literals (bind recursive export re-export nest nest-one host)
    [v:nonref-id
     (ref (pvar (attribute v) (lookup-pvar (attribute v))))]
    [(bind v:nonref-id ...+)
     (group
      (for/list ([v (attribute v)])
        (bind
         this-syntax
         (elaborate-pvar v
                         (s* bindclass-rep)
                         "binding class"))))]
    [(recursive v:nonref-id ...+)
     (rec
         this-syntax
       (for/list ([v (attribute v)])
         (elaborate-pvar v
                         (s* nonterm-rep [variant-info (s* two-pass-nonterm-info)])
                         "two-pass nonterminal")))]
    [(re-export v:nonref-id ...+)
     (re-export
      this-syntax
      (for/list ([v (attribute v)])
        (elaborate-pvar v
                        (s* nonterm-rep [variant-info (s* two-pass-nonterm-info)])
                        "two-pass nonterminal")))]
    [(export v:nonref-id ...+)
     (group
      (for/list ([v (attribute v)])
        (export
         this-syntax
         (elaborate-pvar v
                         (s* bindclass-rep)
                         "binding class"))))]
    [(nest v:nonref-id spec:bspec-term)
     (nest
      this-syntax
      (elaborate-pvar (attribute v)
                      (s* nonterm-rep [variant-info (s* nesting-nonterm-info)])
                      "nesting nonterminal")
      (elaborate-bspec (attribute spec)))]
    [(nest-one v:nonref-id spec:bspec-term)
     (nest-one
      this-syntax
      (elaborate-pvar (attribute v)
                      (s* nonterm-rep [variant-info (s* nesting-nonterm-info)])
                      "nesting nonterminal")
      (elaborate-bspec (attribute spec)))]
    [(host v:nonref-id)
     (suspend
      this-syntax
      (pvar (attribute v) (lookup-pvar (attribute v))))]
    [(~braces spec ...)
     (scope
      this-syntax
      (group (map elaborate-bspec (attribute spec))))]
    [(~brackets spec ...)
     (group (map elaborate-bspec (attribute spec)))]))


;; Elaborator helpers

(define-syntax-rule
  (elaborate-pvar v-e pattern expected-str-e)
  (elaborate-pvar-rt
   v-e
   (match-lambda
     [pattern #t]
     [_ #f])
   expected-str-e))

(define (elaborate-pvar-rt v info-pred expected-str)
  (let ([info (lookup-pvar v)])
    (if (info-pred info)
        (pvar v info)
        (wrong-syntax v (string-append "expected pattern variable associated with a " expected-str)))))

(define (lookup-pvar v)
  (define binding (lookup v pvar-rep?))
  (when (not binding)
    (if (identifier? (current-syntax-context))
        (wrong-syntax/orig v "binding spec expected a reference to a pattern variable")
        (wrong-syntax v "expected a reference to a pattern variable")))
  (pvar-rep-var-info binding))

;; Infer implicit pvar refs

(define (add-implicit-pvar-refs bspec bound-pvars)
  (define unreferenced-pvars
    (remove*
     (bspec-referenced-pvars bspec)
     bound-pvars
     bound-identifier=?))

  (group (cons bspec (for/list ([v unreferenced-pvars])
                       (ref (pvar v (lookup-pvar v)))))))

(define (bspec-referenced-pvars spec)
  (fold-bspec
   (lambda (spec children)
     (define node-vars
       (match spec
         [(or (s* ref [pvar (pvar v _)])
              (s* bind [pvar (pvar v _)])
              (s* export [pvar (pvar v _)])
              (s* nest [pvar (pvar v _)])
              (s* nest-one [pvar (pvar v _)])
              (s* suspend [pvar (pvar v _)]))
          (list v)]
         [(or (s* rec [pvars (list (pvar vs _) ...)])
              (s* re-export [pvars (list (pvar vs _) ...)]))
          vs]
         [_ '()]))
     (append* node-vars children))
   spec))

;; Flatten groups for easier order analysis

(define (bspec-flatten-groups bspec)
  (map-bspec
   (lambda (spec)
     (match spec
       [(group l) (group (append* (map flat-bspec-top-elements l)))] 
       [_ spec]))
   bspec))

(define (flat-bspec-top-elements el)
  (match el
    [(group l) l]
    [_ (list el)]))

;; Static checks

; Concerns:
;   - (bind x) must occur within a scope.
;       - (nest a [(bind x) x]) shouldn't be legal as we have
;         no static guarantee about the length of a or whether its
;         associated non-terminal installs any scopes.
;       - {(nest a [(bind x) x)])} shouldn't be legal as it would imply binding
;         in a scope created by the nesting non-terminal, and the binding might
;         come after a reference created in the nesting non-terminal.
;   - Bindings should come before rec and references within a scope
;   - Exports may only occur at the top-level of a two-pass non-terminal,
;     and appear before rec and references
;   - Only one `recursive` group should appear in a scope, after bindings and before
;     references.
;
; Resulting contexts:
;   - Unscoped expression context; references only.
;   - Scoped expression context
;       - Bindings
;       - Then one recursive
;       - Then references
;   - Exporting context
;       - Exports
;       - Then re-exports
;       - Then references
; The body of a nest is an unscoped expression context, but ideally should have
; special error messages related to nest.
;
; As a grammar:
;
; one-pass-spec: unscoped-spec
; two-pass-spec: (seq (* (export _)) (* (rec _)) refs+subexps)
; unscoped-spec: refs+subexps
; refs+subexps: (* (or (ref _) (nest _ unscoped-spec) (nest-one _ unscoped-spec) (scope scoped-spec)))
; scoped-spec:   (seq (* (bind _)) (? (rec _)) refs+subexps)
;
; The implementation below separately implements refs+subexps for each context in which it occurs to
; provide specific error messages.
;

(define (check-sequence f specs)
  (if (null? specs)
      (void)
      (f (car specs) (cdr specs))))

(define (binding-scope-error stx)
  (wrong-syntax/orig stx "variable binding must occur within a scope"))

(define (export-context-error stx)
  (wrong-syntax/orig stx "exports may only occur at the top-level of a two-pass binding spec"))

(define (re-export-context-error stx)
  (wrong-syntax/orig stx "re-exports may only occur at the top-level of a two-pass binding spec"))

; spec -> (void) or raised syntax error
(define (check-order/unscoped-expression spec)
  (define (refs+subexps spec)
    (match spec
      [(or (s* ref) (s* suspend)) (void)]
      [(and (s* bind) (with-stx stx))
       (binding-scope-error stx)]
      [(and (s* rec) (with-stx stx))
       (wrong-syntax/orig stx "recursive binding groups must occur within a scope")]
      [(and (s* export) (with-stx stx))
       (export-context-error stx)]
      [(and (s* re-export) (with-stx stx))
       (re-export-context-error stx)]
      [(or (s* nest [spec s])
           (s* nest-one [spec s]))
       (check-order/unscoped-expression s)]
      [(s* scope [spec s])
       (check-order/scoped-expression s)]))
  (map refs+subexps (flat-bspec-top-elements spec)))

(define (check-order/scoped-expression spec)
  (define (bindings spec specs)
    (match spec
      [(s* bind)
       (check-sequence bindings specs)]
      [(and (s* export) (with-stx stx))
       (export-context-error stx)]
      [(and (s* re-export) (with-stx stx))
       (re-export-context-error stx)]
      [(s* rec)
       (check-sequence no-more-recs specs)]
      [_ (check-sequence refs+subexps (cons spec specs))]))

  (define (no-more-recs spec specs)
    (match spec
      [(and (s* rec) (with-stx stx))
       (wrong-syntax/orig stx "only one recursive binding group may appear in a scope")]
      [_ (check-sequence refs+subexps (cons spec specs))]))

  (define (refs+subexps spec specs)
    (match spec
      [(and (s* bind) (with-stx stx))
       (wrong-syntax/orig stx "bindings must appear first within a scope")]
      [(and (s* export) (with-stx stx))
       (export-context-error stx)]
      [(and (s* re-export) (with-stx stx))
       (re-export-context-error stx)]
      [(and (s* rec) (with-stx stx))
       (wrong-syntax/orig stx "a recursive binding group must appear before references and subexpressions")]
      [(or (s* ref) (s* suspend))
       (check-sequence refs+subexps specs)]
      [(or (s* nest [spec s])
           (s* nest-one [spec s]))
       (check-order/unscoped-expression s)
       (check-sequence refs+subexps specs)]
      [(s* scope [spec s])
       (check-order/scoped-expression s)
       (check-sequence refs+subexps specs)]))

  (check-sequence bindings (flat-bspec-top-elements spec)))

(define (check-order/two-pass spec)
  (define (exports spec specs)
    (match spec
      [(and (s* export) (with-stx stx))
       (check-sequence exports specs)]
      [_ (check-sequence re-exports (cons spec specs))]))

  (define (re-exports spec specs)
    (match spec
      [(s* re-export)
       (check-sequence re-exports specs)]
      [_
       (check-sequence refs+subexps (cons spec specs))]))
  
  (define (refs+subexps spec specs)
    (match spec
      [(and (s* bind) (with-stx stx))
       (binding-scope-error stx)]
      [(and (s* export) (with-stx stx))
       (wrong-syntax/orig stx "exports must appear first in a two-pass spec")]
      [(and (s* re-export) (with-stx stx))
       (wrong-syntax/orig stx "re-exports must occur before references and subexpressions")]
      [(and (s* rec) (with-stx stx))
       (wrong-syntax/orig stx "recursively-bound subexpressions must occur within a scope")]
      [(or (s* ref) (s* suspend))
       (check-sequence refs+subexps specs)]
      [(or (s* nest [spec s])
           (s* nest-one [spec s]))
       (check-order/unscoped-expression s)
       (check-sequence refs+subexps specs)]
      [(s* scope [spec s])
       (check-order/scoped-expression s)
       (check-sequence refs+subexps specs)]))

  (check-sequence exports (flat-bspec-top-elements spec)))

(define (invariant-error who)
  (error who "should be caught by check-order functions"))

(define (compile-bspec-term/single-pass spec)
  (match spec
    [(ref (pvar v info))
     (match info
       [(nonterm-rep (simple-nonterm-info exp-proc))
        #`(subexp '#,v #,exp-proc)]
       [(bindclass-rep description _ pred)
        #`(ref '#,v #,pred #,(string-append "not bound as " description))]
       [(nested-binding)
        #`(nested)]
       [(nonterm-rep (nesting-nonterm-info _))
        (wrong-syntax/orig v "nesting nonterminals may only be used with `nest`")]
       [(nonterm-rep (two-pass-nonterm-info _ _))
        (wrong-syntax/orig v "two-pass nonterminals may only be used with `recursive` and `re-export`")]
       [(or (? stxclass?) (? has-stxclass-prop?))
        #`(group (list))])]
    [(suspend _ (pvar v info))
     #`(suspend '#,v)]
    [(bind _ (pvar v (bindclass-rep _ constr _)))
     #`(bind '#,v #,constr)]
    [(rec _ pvars)
     (with-syntax ([(s-cp1 ...) (for/list ([pv pvars])
                                  (match-define (pvar v (nonterm-rep (two-pass-nonterm-info pass1-expander _))) pv)
                                  #`(subexp '#,v #,pass1-expander))]
                   [(s-cp2 ...) (for/list ([pv pvars])
                                  (match-define (pvar v (nonterm-rep (two-pass-nonterm-info _ pass2-expander))) pv)
                                  #`(subexp '#,v #,pass2-expander))])
       #`(group (list s-cp1 ... s-cp2 ...)))]
    [(export _ (pvar v _))
     (invariant-error 'compile-bspec-term/single-pass)]
    [(re-export _ (pvar v _))
     (invariant-error 'compile-bspec-term/single-pass)]
    [(nest _ (pvar v info) spec)
     (match info
       [(nonterm-rep (nesting-nonterm-info expander))
        (with-syntax ([spec-c (compile-bspec-term/single-pass spec)])
          #`(nest '#,v #,expander spec-c))])]
    [(nest-one _ (pvar v info) spec)
     (match info
       [(nonterm-rep (nesting-nonterm-info expander))
        (with-syntax ([spec-c (compile-bspec-term/single-pass spec)])
          #`(nest-one '#,v #,expander spec-c))])]
    [(scope _ spec)
     (with-syntax ([spec-c (compile-bspec-term/single-pass spec)])
       #'(scope spec-c))]
    [(group specs)
     (with-syntax ([(spec-c ...) (map compile-bspec-term/single-pass specs)])
       #'(group (list spec-c ...)))]))

(define no-op #'(group (list)))

(define (compile-bspec-term/pass1 spec)
  (match spec
    [(bind _ (pvar v _))
     (invariant-error 'compile-bspec-term/pass1)]
    [(group specs)
     (with-syntax ([(spec-c ...) (map compile-bspec-term/pass1 specs)])
       #'(group (list spec-c ...)))]
    
    [(or (ref _)
         (nest _ _ _)
         (nest-one _ _ _)
         (scope _ _)
         (suspend _ _))
     no-op]
    
    [(export _ (pvar v (bindclass-rep _ constr _)))
     #`(bind '#,v #,constr)]
    [(re-export _ pvars)
     (with-syntax ([(s-c ...) (for/list ([pv pvars])
                                (match-define (pvar v (nonterm-rep (two-pass-nonterm-info pass1-expander _))) pv)
                                #`(subexp '#,v #,pass1-expander))])
       #`(group (list s-c ...)))]))

(define (compile-bspec-term/pass2 spec)
  (match spec
    [(bind _ (pvar v _))
     (invariant-error 'compile-bspec-term/pass2)]
    [(group specs)
     (with-syntax ([(spec-c ...) (map compile-bspec-term/pass2 specs)])
       #'(group (list spec-c ...)))]

    [(or (ref _)
         (nest _ _ _)
         (nest-one _ _ _)
         (scope _ _)
         (suspend _ _))
     (compile-bspec-term/single-pass spec)]
    
    [(export _ _) no-op]
    [(re-export _ pvars)
     (with-syntax ([(s-c ...) (for/list ([pv pvars])
                                (match-define (pvar v (nonterm-rep (two-pass-nonterm-info _ pass2-expander))) pv)
                                #`(subexp '#,v #,pass2-expander))])
       #`(group (list s-c ...)))]))