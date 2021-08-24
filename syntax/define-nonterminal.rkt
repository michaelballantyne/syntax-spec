#lang racket/base

(provide define-binding-class
         define-extension-class
         define-nonterminals
         define-nonterminal
         (for-syntax
          binding-class-predicate
          binding-class-constructor
          nonterminal-expander))

(require "../syntax-spec/spec.rkt"
         "../syntax-spec/expand.rkt"
         "../binding-spec/spec.rkt"
         "../binding-spec/expand.rkt"
         
         (for-syntax racket/base
                     racket/string
                     racket/list
                     racket/syntax
                     syntax/parse
                     ee-lib))



(module stxclasses racket/base
  (provide production-spec ref-id nonref-id)
  
  (require
    racket/string
    racket/list
    syntax/parse
    racket/syntax)
  
  (define (has:? id)
    (string-contains?
     (symbol->string (syntax-e id))
     ":"))
  
  (define (split: id)
    (define strs
      (string-split (symbol->string (syntax-e id)) ":" #:trim? #f))
    (values (format-id id (first strs))
            (format-id id (second strs))))
  
  (define-syntax-class ref-id
    (pattern name:id #:when (has:? #'name)
             #:do [(define-values (var ref) (split: #'name))]
             #:attr var var
             #:attr ref ref))
  
  (define-syntax-class nonref-id
    (pattern name:id #:when (not (has:? #'name))))
  
  (define-splicing-syntax-class production-spec
    #:attributes (form-name sspec bspec)
    (pattern (~seq (~and (name:nonref-id . _) sspec) (~optional (~seq #:binding bspec)))
             #:attr form-name #'name)
    (pattern (~seq sspec (~optional (~seq #:binding bspec)))
             #:attr form-name #f)))

(module errors racket/base
  (provide error-as-expression)
  
  (define (error-as-expression message)
    (lambda (stx)
      (raise-syntax-error
       #f
       message))))

(module env-reps racket/base
  (provide (struct-out bindclass-rep)
           (struct-out extclass-rep)
           (struct-out nonterm-rep))

  (require syntax/parse
           (for-template racket/base))

  (require (submod ".." errors))
  
  (define (nonterm-lang-error-as-expression type)
    (error-as-expression
     (string-append
      type
      " may only be referenced in nonterminal specifications")))
  
  (struct bindclass-rep (description constr pred)
    #:property prop:procedure
    (nonterm-lang-error-as-expression "binding classes"))
  (struct extclass-rep (constr pred acc)
    #:property prop:procedure
    (lambda (s stx)
      (syntax-parse stx
        [(_ e)
         #`(#,(extclass-rep-constr s) e)])))
  (struct nonterm-rep (exp-proc)
    #:property prop:procedure
    (nonterm-lang-error-as-expression "nonterminals")))

(module expander racket/base
  (provide nonterminal-expander)
  
  (require
    "../syntax-spec/spec.rkt"
    "../syntax-spec/expand.rkt"
    "../binding-spec/spec.rkt"
    "../binding-spec/expand.rkt"
    syntax/parse
    ee-lib
    (for-syntax racket/base
                racket/list
                racket/match
                syntax/parse
                syntax/stx
                syntax/parse/class/paren-shape
                syntax/id-table
                syntax/id-set
                ee-lib
                (submod ".." stxclasses)
                (submod ".." env-reps)
                (only-in syntax/parse/private/residual-ct stxclass? has-stxclass-prop?)))
  
  (define-syntax nonterminal-expander
    (syntax-parser
      [(_ #:allow-extension (~or extclass:id #f)
          #:description description
          (prod:production-spec) ...)
       (with-syntax ([(prod-pat ...) (stx-map generate-pattern #'(prod.sspec ...))]
                     [(sspec-e ...) (stx-map compile-sspec #'(prod.sspec ...))]
                     [(bspec-e ...) (stx-map compile-bspec #'(prod ...))]
                     [macro-clause
                      (if (attribute extclass)
                          
                          (let ([ext-info (lookup #'extclass extclass-rep?)])
                            (when (not ext-info)
                              (raise-syntax-error #f "not bound as extension class" #'extclass))
                            (with-syntax ([m-pred (extclass-rep-pred ext-info)]
                                          [m-acc (extclass-rep-acc ext-info)])
                              #'[(m:id . _)
                                 #:do [(define binding (lookup #'m m-pred))]
                                 #:when binding
                                 (recur
                                     (apply-as-transformer (m-acc binding)
                                                           #'m
                                                           'definition
                                                           this-syntax))]))
                          '[_ #:when #f this-syntax])])
         #'(lambda (stx-a)
             (let recur ([stx stx-a])
               (syntax-parse stx
                 macro-clause
                 [prod-pat
                  (nonterminal-expander-rt
                   stx
                   sspec-e
                   bspec-e)]
                 ...
                 [_ (raise-syntax-error
                     #f
                     (string-append "not a " (#%datum . description))
                     this-syntax)]))))]))

  (begin-for-syntax
    (define (generate-pattern stx)
      (define generate-pattern-form
        (syntax-parser
          #:context 'generate-pattern-form
          [(name:nonref-id . term)
           (with-syntax ([term-c (generate-pattern-term #'term)])
             #'((~literal name) ~! . term-c))]
          [_ (generate-pattern-term this-syntax)]))
      (define generate-pattern-term
        (syntax-parser
          #:context 'generate-pattern-term
          [()
           #'()]
          [(t1 . t2)
           (with-syntax ([t1-c (generate-pattern-term #'t1)]
                         [t2-c (generate-pattern-term #'t2)])
             #'(t1-c . t2-c))]
          [r:ref-id
           #:do [(define binding (lookup #'r.ref bindclass-rep?))]
           #:when binding
           #'_:id]
          [r:ref-id
           #:do [(define binding (lookup #'r.ref nonterm-rep?))]
           #:when binding
           #'_]
          [r:ref-id
           #:do [(define binding (lookup #'r.ref (lambda (v) (or (stxclass? v)
                                                                 (has-stxclass-prop? v)))))]
           #:when binding
           #'(~var _ r.ref)]))
      
      (generate-pattern-form stx))
    
    (define (compile-sspec stx)
      (define compile-sspec-form
        (syntax-parser
          #:context 'compile-sspec-form
          [(name:nonref-id . d)
           (with-syntax ([d-c (compile-sspec-term #'d)])
             #'(cons (pany) d-c))]
          [_ (compile-sspec-term this-syntax)]))
      
      (define compile-sspec-term
        (syntax-parser
          #:context 'compile-sspec-term
          [(a . d)
           (with-syntax ([a-c (compile-sspec-term #'a)]
                         [d-c (compile-sspec-term #'d)])
             #'(cons a-c d-c))]
          [r:ref-id
           #'(pvar 'r.var)]
          [lit
           #''lit]))
      
      (compile-sspec-form stx))

    (define (extract-var-mapping stx)
      (define res (make-immutable-bound-id-table))

      (let rec ([stx stx])
        (syntax-parse stx
          #:context 'extract-var-mapping
          [(a . d)
           (rec #'a)
           (rec #'d)]
          [r:ref-id
           (define binding (lookup #'r.ref
                                   (lambda (v)
                                     (or (bindclass-rep? v)
                                         (nonterm-rep? v)
                                         (stxclass? v)
                                         (has-stxclass-prop? v)))))
           (when (not binding)
             (raise-syntax-error #f "not a binding class, syntax class, or nonterminal" #'r.ref))
           (when (or (bindclass-rep? binding)
                     (nonterm-rep? binding))
             (set! res (bound-id-table-set res #'r.var binding)))]
          [_ (void)]))

      res)
    
    (define (compile-bspec stx)
      (define/syntax-parse (prod:production-spec) stx)

      (define varmap (extract-var-mapping #'prod.sspec))

      (define referenced-vars
        (syntax-parser
          #:datum-literals (! ^)
          [_ this-syntax]))

      (define compile-bspec-term
        (syntax-parser
          #:context 'compile-bspec-term
          #:datum-literals (! ^)
          [v:nonref-id
           (define binding (bound-id-table-ref varmap #'v #f))
           (when (not binding)
             (raise-syntax-error #f "no corresponding pattern variable" #'v))
           (match binding
             [(nonterm-rep exp-proc)
              #`(subexp 'v #,exp-proc)]
             [(bindclass-rep description _ pred)
              #`(ref 'v #,pred #,(string-append "not bound as " description))])]
          [(! v:nonref-id ...+)
           (with-syntax
               ([(v-c ...)
                 (for/list ([v (syntax->list #'(v ...))])
                   (define binding (bound-id-table-ref varmap v #f))
                   (when (not binding)
                     (raise-syntax-error #f "no corresponding pattern variable" v))
                   (match binding
                     [(bindclass-rep _ constr _)
                      #`(bind '#,v #,constr)]))])
             #'(group (list v-c ...)))]
          [(^ v:ref-id ...+)
           (raise-syntax-error #f "^ not implemented yet" this-syntax)]
          [(~braces spec ...)
           (with-syntax ([(spec-c ...) (stx-map compile-bspec-term #'(spec ...))])
             #'(scope (group (list spec-c ...))))]
          [(~brackets spec ...)
           (with-syntax ([(spec-c ...) (stx-map compile-bspec-term #'(spec ...))])
             #'(group (list spec-c ...)))]))

      (define referenced-pvars
        (syntax-parser
          #:context 'referenced-pvars
          #:datum-literals (! ^)
          [v:nonref-id
           (list #'v)]
          [(! v:nonref-id ...+)
           (syntax->list #'(v ...))]
          [(^ v:ref-id ...+)
           (syntax->list #'(v ...))]
          [(~braces spec ...)
           (flatten (stx-map referenced-pvars #'(spec ...)))]
          [(~brackets spec ...)
           (flatten (stx-map referenced-pvars #'(spec ...)))]))
        
      
      (define/syntax-parse bspec #'(~? prod.bspec []))
      
      (define/syntax-parse (unreferenced-pvars ...)
        (bound-id-set->list
         (bound-id-set-subtract
          (immutable-bound-id-set (bound-id-table-keys varmap))
          (immutable-bound-id-set (referenced-pvars #'bspec)))))
      
      (compile-bspec-term #'[unreferenced-pvars ... bspec]))
    )

  (define (nonterminal-expander-rt stx
                                   stx-spec
                                   binding-spec)
    (define vmap (deconstruct stx stx-spec))
    (define vmap^ (simple-expand binding-spec vmap))
    (reconstruct vmap^ stx stx-spec)))

(module definitions racket/base
  (provide define-binding-class
           define-extension-class
           define-nonterminals
           define-nonterminal)
  
  (require
    (for-syntax
     racket/base
     racket/list
     racket/syntax
     syntax/parse
     (submod ".." stxclasses)
     (submod ".." expander)
     (submod ".." errors))
    (for-meta 2
              racket/base
              (submod ".." env-reps)))

  (define-syntax define-binding-class
    (syntax-parser
      [(_ name:id description:expr)
       (with-syntax ([sname (format-id #'here "~a-var" #'name)]
                     [sname-pred (format-id #'here "~a-var?" #'name)])
         #'(begin-for-syntax
             (struct sname [])
             (define-syntax name
               (bindclass-rep
                (#%datum . description)
                (quote-syntax sname)
                (quote-syntax sname-pred)))))]))

  (define-syntax define-extension-class
    (syntax-parser
      [(_ name:id)
       (with-syntax ([sname (format-id #'here "~a" #'name)]
                     [sname-pred (format-id #'here "~a?" #'name)]
                     [sname-acc (format-id #'here "~a-transformer" #'name)])
         #'(begin-for-syntax
             (struct sname [transformer])
             (define-syntax name
               (extclass-rep (quote-syntax sname)
                             (quote-syntax sname-pred)
                             (quote-syntax sname-acc)))))]))
  
  (define-syntax define-nonterminals
    (syntax-parser
      [(_ [name:id
           #:description description:string
           (~optional (~seq #:allow-extension extclass:id))
           #;(~optional (~seq #:define-literal-set set-name:id))
           prod:production-spec
           ...]
          ...)

       (let ([maybe-dup-id (check-duplicate-identifier
                            (filter (lambda (x) x)
                                    (flatten (attribute prod.form-name))))])
         (when maybe-dup-id
           (wrong-syntax maybe-dup-id "duplicate form name")))

       (define/syntax-parse expander-name (generate-temporary #'expander))
       
       #'(begin
           ; TODO improve message
           (begin
             (~? (define-syntax prod.form-name
                   (error-as-expression "may not be used as an expression"))
                 (begin))
             ...)
           ...
           (begin-for-syntax
             #;(begin
               (~? (define-literal-set set-name (prod.form-name ...))
                   (begin))
               ...)
             (define-syntax name (nonterm-rep #'expander-name))
             ...
             (define expander-name
               (nonterminal-expander
                #:allow-extension (~? extclass #f)
                #:description description
                prod ...))
             ...)
           )]))
  (define-syntax define-nonterminal
    (syntax-parser
      [(_ . spec)
       #'(define-nonterminals
           spec)]))
  )

(require (submod "." definitions))

(require (for-meta 2
                   racket/base
                   ee-lib
                   syntax/parse
                   syntax/parse/experimental/reflect
                   (submod "." env-reps)))

(begin-for-syntax
  (define-syntax binding-class-constructor
    (syntax-parser
      [(_ bindclass:id)
       (define binding (lookup #'bindclass bindclass-rep?))
       (when (not binding)
         (raise-syntax-error #f "not bound as binding class" #'bindclass))
       (bindclass-rep-constr binding)]))
  
  (define-syntax binding-class-predicate
    (syntax-parser
      [(_ bindclass:id)
       (define binding (lookup #'bindclass bindclass-rep?))
       (when (not binding)
         (raise-syntax-error #f "not bound as binding class" #'bindclass))
       (bindclass-rep-pred binding)]))
  
  (define-syntax nonterminal-expander
    (syntax-parser
      [(_ nonterm:id)
       (define binding (lookup #'nonterm nonterm-rep?))
       (when (not binding)
         (raise-syntax-error #f "not bound as nonterminal" #'nonterm))
       (nonterm-rep-exp-proc binding)])))
