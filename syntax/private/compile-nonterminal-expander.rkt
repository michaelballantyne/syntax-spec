#lang racket/base

(provide compile-nonterminal-expander)
  
(require racket/base
         racket/list
         racket/match
         syntax/parse
         syntax/stx
         syntax/parse/class/paren-shape
         syntax/id-table
         syntax/id-set
         ee-lib
         "syntax-classes.rkt"
         "env-reps.rkt"
         
         (only-in syntax/parse/private/residual-ct stxclass? has-stxclass-prop?)

         (for-template racket/base
                       "../../binding-spec/spec.rkt"
                       "../../binding-spec/expand.rkt"
                       "rebind-pattern-vars.rkt"
                       syntax/parse
                       ee-lib))

(define (compile-nonterminal-expander stx)
  (syntax-parse stx
    [(#:allow-extension (~or extclass:id #f)
      #:description description
      (prod:production-spec) ...)
     (with-syntax ([((v ...) ...) (stx-map extract-pvars #'(prod.sspec ...))]
                   [(prod-pat ...) (stx-map generate-pattern #'(prod.sspec ...))]
                   [(bspec-e ...) (stx-map compile-bspec #'(prod ...))]
                   [(template ...) (stx-map generate-template #'(prod.sspec ...))]
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
                (let* ([in (hash (~@ 'v (pattern-var-value v)) ...)]
                       [out (simple-expand bspec-e in)])
                  (rebind-pattern-vars ([(v ...) (values (hash-ref out 'v) ...)])
                     #'template))]
               ...
               [_ (raise-syntax-error
                   #f
                   (string-append "not a " (#%datum . description))
                   this-syntax)]))))]))

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
      [(~literal ...)
       #'(... ...)]
      [(t1 . t2)
       (with-syntax ([t1-c (generate-pattern-term #'t1)]
                     [t2-c (generate-pattern-term #'t2)])
         #'(t1-c . t2-c))]
      [r:ref-id
       #:do [(define binding (lookup #'r.ref bindclass-rep?))]
       #:when binding
       #'(~var r.var id)]
      [r:ref-id
       #:do [(define binding (lookup #'r.ref nonterm-rep?))]
       #:when binding
       #'r.var]
      [r:ref-id
       #:do [(define binding (lookup #'r.ref (lambda (v) (or (stxclass? v)
                                                             (has-stxclass-prop? v)))))]
       #:when binding
       #'(~var r.var r.ref)]))
      
  (generate-pattern-form stx))

(define (generate-template stx)
  (define generate-template-form
    (syntax-parser
      #:context 'generate-pattern-form
      [(name:nonref-id . term)
       (with-syntax ([term-c (generate-template-term #'term)])
         #'(name . term-c))]
      [_ (generate-template-term this-syntax)]))
  (define generate-template-term
    (syntax-parser
      #:context 'generate-template-term
      [()
       #'()]
      [(~literal ...)
       #'(... ...)]
      [(t1 . t2)
       (with-syntax ([t1-c (generate-template-term #'t1)]
                     [t2-c (generate-template-term #'t2)])
         #'(t1-c . t2-c))]
      [r:ref-id
       #:do [(define binding (lookup #'r.ref bindclass-rep?))]
       #:when binding
       #'r.var]
      [r:ref-id
       #:do [(define binding (lookup #'r.ref nonterm-rep?))]
       #:when binding
       #'r.var]
      [r:ref-id
       #:do [(define binding (lookup #'r.ref (lambda (v) (or (stxclass? v)
                                                             (has-stxclass-prop? v)))))]
       #:when binding
       #'r.var]))
      
  (generate-template-form stx))

(define (extract-pvars stx)
  (bound-id-table-keys (extract-var-mapping stx)))

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

