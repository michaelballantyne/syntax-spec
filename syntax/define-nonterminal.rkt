#lang racket/base

(provide define-binding-class
         define-extension-class
         define-nonterminals
         define-nonterminal
         (for-syntax binding-class-predicate))

(require "../syntax-spec/spec.rkt"
         "../syntax-spec/expand.rkt"
         "../binding-spec/spec.rkt"
         "../binding-spec/expand.rkt"
         
         (for-syntax racket/base
                     racket/string
                     racket/list
                     racket/syntax
                     syntax/parse
                     syntax/parse/class/paren-shape
                     syntax/parse/experimental/reflect
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
    (pattern (~seq (~and (name:nonref-id . _) sspec) #:binding bspec)
             #:attr form-name #'name)
    #;(pattern _
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
    (nonterm-lang-error-as-expression "extension classes"))
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
    (for-syntax racket/base
                racket/match
                syntax/parse
                syntax/stx
                syntax/parse/class/paren-shape
                syntax/id-table
                ee-lib
                (submod ".." stxclasses)
                (submod ".." env-reps)))
  
  (define-syntax nonterminal-expander
    (syntax-parser
      [(_ #:allow-extension (~or vclass:id #f)
          #:else f
          (prod:production-spec) ...)
       (with-syntax ([(prod-pat ...) (stx-map generate-pattern #'(prod.sspec ...))]
                     [(sspec-e ...) (stx-map compile-sspec #'(prod.sspec ...))]
                     [(bspec-e ...) (stx-map compile-bspec #'(prod ...))])
         #'(lambda (stx)
             (syntax-parse stx
               [prod-pat
                (nonterminal-expander-rt
                 stx
                 sspec-e
                 bspec-e)]
               ...
               [_ (f stx)])))]))

  (begin-for-syntax
    (define (generate-pattern stx)
      (define generate-pattern-form
        (syntax-parser
          [(name:nonref-id term ...)
           (with-syntax ([(term-c ...) (stx-map generate-pattern-term #'(term ...))])
             #'((~literal name) ~! term-c ...))]))
      (define generate-pattern-term
        (syntax-parser
          [(term ...)
           (with-syntax ([(term-c ...) (stx-map generate-pattern-term #'(term ...))])
             #'(term-c ...))]
          [r:ref-id
           (define binding (lookup #'r.ref bindclass-rep?))
           (if binding
               #'_:id
               #'_)]))
      
      (generate-pattern-form stx))
    
    (define (compile-sspec stx)
      (define compile-sspec-form
        (syntax-parser
          #:context 'compile-sspec-form
          [(name:nonref-id . d)
           (with-syntax ([d-c (compile-sspec-term #'d)])
             #'(cons (pany) d-c))]))
      
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
                                         (nonterm-rep? v)))))
           (when (not binding)
             (raise-syntax-error #f "not bound as a binding class or nonterminal" #'r.ref))
           (set! res (bound-id-table-set res #'r.var binding))]
          [_ (void)]))

      res)
    
    (define (compile-bspec stx)
      (define/syntax-parse (prod:production-spec) stx)

      (define varmap (extract-var-mapping #'prod.sspec))

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

      (compile-bspec-term #'prod.bspec))
    )

  (define (nonterminal-expander-rt stx
                                   stx-spec
                                   binding-spec)
    (define vmap (deconstruct stx stx-spec))
    (define vmap^ (simple-expand binding-spec vmap))
    (reconstruct vmap stx stx-spec)))

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
       (with-syntax ([sname (format-id #'here "~a-macro" #'name)]
                     [sname-pred (format-id #'here "~a-macro?" #'name)]
                     [sname-acc (format-id #'here "~a-transformer?" #'name)])
         #'(begin-for-syntax
             (struct sname [transformer])
             (define-syntax name
               (extclass-rep (quote-syntax sname)
                             (quote-syntax sname-pred)
                             (quote-syntax sname-acc)))))]))
  
  (define-syntax define-nonterminals
    (syntax-parser
      [(_ [name:id
           #:expander expander-name:id
           #:else f:id
           (~optional (~seq #:allow-extension eclass:id))
           (~optional (~seq #:define-literal-set set-name:id))
           prod:production-spec
           ...]
          ...)

       (let ([maybe-dup-id (check-duplicate-identifier (flatten (attribute prod.name)))])
         (when maybe-dup-id
           (wrong-syntax maybe-dup-id "duplicate form name")))

       #'(begin
           ; TODO improve message
           (begin
             (define-syntax prod.name (error-as-expression "may not be used as an expression"))
             ...)
           ...
           (begin-for-syntax
             (begin
               (~? (define-literal-set set-name (prod.name) ...)
                   (begin))
               ...)
             (define-syntax name (nonterm-rep #'expander-name))
             ...
             (define expander-name
               (nonterminal-expander
                #:allow-extension (~? eclass #f)
                #:else f
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

(require (for-syntax (submod "." env-reps)))

(require (for-meta 2 racket/base ee-lib syntax/parse (submod "." env-reps)))
(begin-for-syntax
  (define (literal-nonterminal stxclass)
    (nonterm-rep
     (syntax-parser
       [(~reflect v (stxclass))
        #'v])))

  (define-syntax binding-class-predicate
    (syntax-parser
      [(_ bindclass:id)
       (define binding (lookup #'bindclass bindclass-rep?))
       (when (not binding)
         (raise-syntax-error #f "not bound as binding class" #'bindclass))
       (bindclass-rep-pred binding)])))
       

(define-syntax-rule
  (define-literal-nonterminal name stxclass-id)
  (define-syntax name
    (literal-nonterminal (reify-syntax-class stxclass-id))))

(define-literal-nonterminal boolean boolean)
(define-literal-nonterminal number number)
(define-literal-nonterminal string string)