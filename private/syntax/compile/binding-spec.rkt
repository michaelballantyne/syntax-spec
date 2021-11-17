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
  (define bspec (or maybe-bspec #'[]))
  (define bspec-with-implicits (add-implicit-pvar-refs bspec bound-pvars))

  (define variant-compiler
    (syntax-parse variant
      [(~or #:simple (#:nesting _)) compile-bspec-term/single-pass]
      [#:pass1 compile-bspec-term/pass1]
      [#:pass2 compile-bspec-term/pass2]))

  (variant-compiler bspec-with-implicits))

(define (add-implicit-pvar-refs bspec bound-pvars)
  (define/syntax-parse (unreferenced-pvars ...)
    (remove*
     (bspec-referenced-pvars bspec)
     bound-pvars
     bound-identifier=?))

  #`[#,bspec unreferenced-pvars ...])

(define bspec-referenced-pvars
  (syntax-parser
    #:datum-literals (! rec ^ nest)
    [v:nonref-id
     (list #'v)]
    [(! v:nonref-id ...+)
     (attribute v)]
    [(^ v:nonref-id ...+)
     (attribute v)]
    [(rec v:nonref-id ...+)
     (attribute v)]
    [(nest v:nonref-id e:bspec-term)
     (cons (attribute v) (bspec-referenced-pvars (attribute e)))]
    [(~braces spec:bspec-term ...)
     (flatten (map bspec-referenced-pvars (attribute spec)))]
    [(~brackets spec:bspec-term ...)
     (flatten (map bspec-referenced-pvars (attribute spec)))]))

(define (lookup-pvar v)
  (define binding (lookup v pvar-rep?))
  (when (not binding)
    (if (identifier? (current-syntax-context))
        (wrong-syntax/orig v "binding spec expected a reference to a pattern variable")
        (wrong-syntax v "expected a reference to a pattern variable")))
  (pvar-rep-var-info binding))

(define compile-bspec-term/single-pass
  (syntax-parser
    #:context 'compile-bspec-term/single-pass
    #:datum-literals (! rec ^ nest)
    [v:nonref-id
     (match (lookup-pvar #'v)
       [(nonterm-rep _ (simple-nonterm-info exp-proc))
        #`(subexp 'v #,exp-proc)]
       [(bindclass-rep description _ pred)
        #`(ref 'v #,pred #,(string-append "not bound as " description))]
       [(nested-binding)
        #`(nested)]
       [(nonterm-rep _ (nesting-nonterm-info _))
        (wrong-syntax/orig #'v "nesting nonterminals may only be used with `nest`")]
       [(or (? stxclass?) (? has-stxclass-prop?))
        #`(group (list))])]
    [(! v:nonref-id ...+)
     (with-syntax
         ([(v-c ...)
           (for/list ([v (syntax->list #'(v ...))])
             (match (lookup-pvar v)
               [(bindclass-rep _ constr _)
                #`(bind '#,v #,constr)]
               [_ (wrong-syntax #'v "expected pattern variable associated with a binding class")]))])
       #'(group (list v-c ...)))]
    [(rec v:nonref-id ...+)
     (define nonterm-infos
       (for/list ([v (syntax->list #'(v ...))])
         (match (lookup-pvar v)
           [(nonterm-rep _ (and info (two-pass-nonterm-info _ _)))
            info]
           [_ (wrong-syntax v "expected pattern variable associated with a two-pass nonterminal")])))
     ;TODO
     (error 'compile-bspec-term/single-pass "rec not implemented yet")]
    [(^ v:nonref-id ...+)
     (wrong-syntax/orig this-syntax "exports may only occur at the top-level of a two-pass binding spec")]
    [(nest v:nonref-id spec)
     (match (lookup-pvar (attribute v))
       [(nonterm-rep _ (nesting-nonterm-info expander))
        (with-syntax ([spec-c (compile-bspec-term/single-pass (attribute spec))])
          #`(nest 'v #,expander spec-c))]
       [_ (wrong-syntax #'v "expected pattern variable assocated with a nesting nonterminal")])]
    [(~braces spec ...)
     (with-syntax ([(spec-c ...) (map compile-bspec-term/single-pass (attribute spec))])
       #'(scope (group (list spec-c ...))))]
    [(~brackets spec ...)
     (with-syntax ([(spec-c ...) (map compile-bspec-term/single-pass (attribute spec))])
       #'(group (list spec-c ...)))]))

; TODO
(define compile-bspec-term/pass1
  (syntax-parser
    #:context 'compile-bspec-term/pass1
    #:datum-literals (! rec ^ nest)
    [v:nonref-id 'TODO]
    [(! v:nonref-id ...+) 'TODO]
    [(rec v:nonref-id ...+) 'TODO]
    [(^ v:nonref-id ...+) 'TODO]
    [(nest v:nonref-id spec) 'TODO]
    [(~braces spec ...) 'TODO]
    [(~brackets spec ...) 'TODO]))

(define compile-bspec-term/pass2
  (syntax-parser
    #:context 'compile-bspec-term/pass2
    #:datum-literals (! rec ^ nest)
    [v:nonref-id 'TODO]
    [(! v:nonref-id ...+) 'TODO]
    [(rec v:nonref-id ...+) 'TODO]
    [(^ v:nonref-id ...+) 'TODO]
    [(nest v:nonref-id spec) 'TODO]
    [(~braces spec ...) 'TODO]
    [(~brackets spec ...) 'TODO]))