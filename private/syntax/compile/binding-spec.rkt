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
         )

(define (compile-bspec maybe-bspec bound-pvars variant)
  (define bspec-stx (or maybe-bspec #'[]))

  (define bspec-elaborated (elaborate-bspec bspec-stx))
  (define bspec-with-implicits (add-implicit-pvar-refs bspec-elaborated bound-pvars))
  (define bspec-flattened (bspec-flatten-groups bspec-with-implicits))

  (define variant-compiler
    (syntax-parse variant
      [(~or #:simple (#:nesting _)) compile-bspec-term/single-pass]
      [#:pass1 compile-bspec-term/pass1]
      [#:pass2 compile-bspec-term/pass2]))

  (variant-compiler bspec-flattened))

;; Elaborated representation; variables are associated with expander-environment information

(struct pvar [id info])

(struct ref [pvar])
(struct bind [pvar])
(struct rec [pvars])
(struct export [pvar])
(struct nest [pvar spec])
(struct scope [spec])
(struct group [specs])

;; Elaborate

(define elaborate-bspec
  (syntax-parser
    #:datum-literals (! rec ^ nest)
    [v:nonref-id
     (ref (pvar (attribute v) (lookup-pvar (attribute v))))]
    [(! v:nonref-id ...+)
     (group
      (for/list ([v (attribute v)])
        (bind
         (elaborate-pvar v
                         (struct* bindclass-rep ())
                         "binding class"))))]
    [(rec v:nonref-id ...+)
     (rec
         (for/list ([v (attribute v)])
           (elaborate-pvar v
                           (struct* nonterm-rep ([variant-info (struct* two-pass-nonterm-info ())]))
                           "two-pass nonterminal")))]
    [(^ v:nonref-id ...+)
     (group
      (for/list ([v (attribute v)])
        (export
         (elaborate-pvar v
                         (struct* bindclass-rep ())
                         "binding class"))))]
    [(nest v:nonref-id spec:bspec-term)
     (nest
      (elaborate-pvar (attribute v)
                      (struct* nonterm-rep ([variant-info (struct* nesting-nonterm-info ())]))
                      "nesting nonterminal")
      (elaborate-bspec (attribute spec)))]
    [(~braces spec ...)
     (scope (group (map elaborate-bspec (attribute spec))))]
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
  (match spec
    [(or (ref (pvar v _))
         (bind (pvar v _))
         (export (pvar v _)))
     (list v)]
    [(rec (list (pvar vs _) ...))
     vs]
    [(nest (pvar v _) spec)
     (cons v (bspec-referenced-pvars spec))]
    [(scope spec)
     (bspec-referenced-pvars spec)]
    [(group specs)
     (apply append (map bspec-referenced-pvars specs))]))

;; Flatten groups for easier order analysis

(define (bspec-flatten-groups bspec)
  (define (flattened-elements el)
    (match el
      [(group l)
       (apply append (map flattened-elements l))]
      [_ (list (bspec-flatten-groups el))]))
  
  (match bspec
    [(nest v spec)
     (nest v (bspec-flatten-groups spec))]
    [(scope spec)
     (scope (bspec-flatten-groups spec))]
    [(group l) (group (flattened-elements bspec))]
    [other other]))

(define (flat-bspec-top-elements el)
  (match el
    [(group l) l]
    [_ (list el)]))

(define (compile-bspec-term/single-pass spec)
  (match spec
    [(ref (pvar v info))
     (match info
       [(nonterm-rep _ (simple-nonterm-info exp-proc))
        #`(subexp '#,v #,exp-proc)]
       [(bindclass-rep description _ pred)
        #`(ref '#,v #,pred #,(string-append "not bound as " description))]
       [(nested-binding)
        #`(nested)]
       [(nonterm-rep _ (nesting-nonterm-info _))
        (wrong-syntax/orig v "nesting nonterminals may only be used with `nest`")]
       [(or (? stxclass?) (? has-stxclass-prop?))
        #`(group (list))])]
    [(bind (pvar v (bindclass-rep _ constr _)))
     #`(bind '#,v #,constr)]
    [(rec pvars)
     (with-syntax ([(s-cp1 ...) (for/list ([pv pvars])
                                  (match-define (pvar v (nonterm-rep _ (two-pass-nonterm-info pass1-expander _))) pv)
                                  #`(subexp '#,v #,pass1-expander))]
                   [(s-cp2 ...) (for/list ([pv pvars])
                                  (match-define (pvar v (nonterm-rep _ (two-pass-nonterm-info _ pass2-expander))) pv)
                                  #`(subexp '#,v #,pass2-expander))])
       #`(group (list s-cp1 ... s-cp2 ...)))]
    [(export (pvar v _))
     (wrong-syntax/orig v "exports may only occur at the top-level of a two-pass binding spec")]
    [(nest (pvar v info) spec)
     (match info
       [(nonterm-rep _ (nesting-nonterm-info expander))
        (with-syntax ([spec-c (compile-bspec-term/single-pass spec)])
          #`(nest '#,v #,expander spec-c))])]
    [(scope spec)
     (with-syntax ([spec-c (compile-bspec-term/single-pass spec)])
       #'(scope spec-c))]
    [(group specs)
     (with-syntax ([(spec-c ...) (map compile-bspec-term/single-pass specs)])
       #'(group (list spec-c ...)))]))

(define no-op #'(group (list)))

; TODO
(define (compile-bspec-term/pass1 spec)
  (match spec
    [(bind (pvar v _))
     (wrong-syntax/orig v "bindings must appear within a scope")]
    [(group specs)
     (with-syntax ([(spec-c ...) (map compile-bspec-term/pass1 specs)])
       #'(group (list spec-c ...)))]
    
    [(or (ref _)
         (nest _ _)
         (scope _))
     no-op]
    
    [(export (pvar v (bindclass-rep _ constr _)))
     #`(bind '#,v #,constr)]
    [(rec pvars)
     (with-syntax ([(s-c ...) (for/list ([pv pvars])
                                (match-define (pvar v (nonterm-rep _ (two-pass-nonterm-info pass1-expander _))) pv)
                                #`(subexp '#,v #,pass1-expander))])
       #`(group (list s-c ...)))]))

(define (compile-bspec-term/pass2 spec)
  (match spec
    [(bind (pvar v _)) (wrong-syntax/orig v "bindings must appear within a scope")]
    [(group specs)
     (with-syntax ([(spec-c ...) (map compile-bspec-term/pass2 specs)])
       #'(group (list spec-c ...)))]

    [(or (ref _)
         (nest _ _)
         (scope _))
     (compile-bspec-term/single-pass spec)]
    
    [(export _) no-op]
    [(rec pvars)
     (with-syntax ([(s-c ...) (for/list ([pv pvars])
                                (match-define (pvar v (nonterm-rep _ (two-pass-nonterm-info _ pass2-expander))) pv)
                                #`(subexp '#,v #,pass2-expander))])
       #`(group (list s-c ...)))]))