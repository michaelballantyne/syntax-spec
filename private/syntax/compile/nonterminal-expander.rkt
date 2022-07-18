#lang racket/base

(provide compile-nonterminal-expander generate-interface-expansion)
  
(require racket/base
         syntax/parse
         syntax/id-set
         racket/syntax
         ee-lib
         "../syntax-classes.rkt"
         "../env-reps.rkt"
         "syntax-spec.rkt"
         "binding-spec.rkt"

         (for-template racket/base
                       "../syntax-classes.rkt"
                       "../../runtime/binding-spec.rkt"
                       "pattern-var-reflection.rkt"
                       syntax/parse
                       racket/syntax
                       ee-lib))

(define (compile-nonterminal-expander stx)
  (syntax-parse stx
    #:context 'compile-nonterminal-expander
    [(variant
      name
      (opts:nonterminal-options)
      prod-arg ...)
     (define (generate-loop prods-stx maybe-nested-id init-stx-id)
       (define/syntax-parse ((prod:production) ...) prods-stx)
       (with-syntax ([prod-clauses (map (lambda (prod)
                                          (generate-prod-clause prod maybe-nested-id (attribute variant) #'recur (attribute opts.space-stx)))
                                        (attribute prod))]
                     [macro-clauses (for/list ([extclass (attribute opts.ext-classes)])
                                      (generate-macro-clause extclass #'recur))]
                     [description (or (attribute opts.description) (symbol->string (syntax-e (attribute name))))]
                     [stx-a init-stx-id])
         #'(let recur ([stx stx-a])
             (syntax-parse stx
               (~@ . macro-clauses)
               (~@ . prod-clauses)
               [_ (wrong-syntax/orig
                   this-syntax
                   (string-append "expected " (#%datum . description)))]))))

     (define (generate-header)
       (syntax-parse (attribute variant)
         [(#:nesting nested-id:id)
          (with-scope sc
            (define id^ (bind! (add-scope (attribute nested-id) sc) (pvar-rep (nested-binding))))
            #`(wrap-hygiene
               (lambda (stx-a)
                 #,(generate-loop (add-scope #'(prod-arg ...) sc) id^ #'stx-a))
               'definition))]
         [_
          #`(wrap-hygiene
             (lambda (stx-a)
               #,(generate-loop #'(prod-arg ...) #f #'stx-a))
             'definition)]))
     
     (generate-header)]))

(define (generate-prod-clause prod-stx maybe-nested-id variant recur-id binding-space-stx)
  (syntax-parse prod-stx
    [(p:rewrite-production)
     ;; Hygiene for rewrite productions only uses a macro introduction
     ;; scope applied to the input and flipped on the output. 
     (with-syntax ([recur recur-id])
       #`[p.pat
          p.parse-body ...
          (recur p.final-body)])]
    [(p:syntax-production)
     (with-scope sc
       (define sspec (add-scope (attribute p.sspec) sc))
       (define maybe-bspec (and (attribute p.bspec) (add-scope (attribute p.bspec) sc)))

       (generate-prod-expansion
        sspec maybe-bspec (and maybe-nested-id (add-scope maybe-nested-id sc)) variant binding-space-stx
        (lambda () #`(syntax/loc this-syntax #,(compile-sspec-to-template sspec)))))]))


(define (generate-prod-expansion sspec maybe-bspec maybe-nested-id variant binding-space-stx generate-body)
  (generate-expansion sspec maybe-bspec maybe-nested-id (list variant) #'expand-function-return binding-space-stx generate-body))

(define (generate-interface-expansion sspec maybe-bspec pass-variants generate-body)
  (generate-expansion sspec maybe-bspec #f pass-variants #'expand-top #f generate-body))

; TODO: lacking hygiene b/t introduced vars and generate-body
(define (generate-expansion sspec maybe-bspec maybe-nested-id pass-variants bspec-evaluator binding-space-stx generate-body)
  (define sspec-pvars (sspec-bind-pvars! sspec))
  (define bound-pvars (if maybe-nested-id
                          (append sspec-pvars (list maybe-nested-id))
                          sspec-pvars))
    
  (with-syntax ([(v ...) sspec-pvars]
                [pattern (compile-sspec-to-pattern sspec binding-space-stx)]
                [(bspec-e ...)
                 (for/list ([variant pass-variants])
                   (compile-bspec maybe-bspec bound-pvars variant))])
    #`[pattern
        (#,bspec-evaluator
         (list bspec-e ...)
         (hash (~@ 'v (attribute v)) ...)
         (lambda (env^)
           (rebind-pattern-vars
            (v ...)
            (values (hash-ref env^ 'v) ...)
            #,(generate-body))))]))


(define (generate-macro-clause extclass recur-id)
  (let ([ext-info (lookup extclass extclass-rep?)])
    (when (not ext-info)
      (wrong-syntax/orig extclass "expected extension class name"))
        
    (with-syntax ([m-pred (extclass-rep-pred ext-info)]
                  [m-acc (extclass-rep-acc ext-info)]
                  [m-space (extclass-rep-binding-space ext-info)]
                  [recur recur-id])
      #'[(~or m:id (m:id . _))
         #:do [(define binding (lookup #'m m-pred #:space 'm-space))]
         #:when binding
         (recur (apply-as-transformer (m-acc binding)
                                      ((in-space 'm-space) #'m)
                                      'definition
                                      this-syntax))])))