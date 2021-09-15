#lang racket/base

(provide compile-nonterminal-expander)
  
(require racket/base
         syntax/parse
         syntax/id-set
         ee-lib
         "../syntax-classes.rkt"
         "../env-reps.rkt"
         "syntax-spec.rkt"
         "binding-spec.rkt"

         (for-template racket/base
                       "../../runtime/binding-spec.rkt"
                       "pattern-var-reflection.rkt"
                       syntax/parse
                       ee-lib))

(define (compile-nonterminal-expander stx)
  (syntax-parse stx
    #:context 'compile-nonterminal-expander
    [(#:description description
      #:allow-extension (extclass ...)
      #:nested-id (~or nested-id:id #f)
      prod-arg ...)
     (with-scope sc
       (define/syntax-parse ((prod:production-spec) ...) (add-scope #'(prod-arg ...) sc))

       (when (attribute nested-id)
         (bind! (add-scope (attribute nested-id) sc) (pvar-rep (continuation-binding #'k))))
     
       (with-syntax ([args (if (attribute nested-id) #'(stx-a k) #'(stx-a))]
                     [prod-clauses (map generate-prod-clause (attribute prod.sspec) (attribute prod.bspec))]
                     [macro-clauses (for/list ([extclass (attribute extclass)])
                                      (generate-macro-clause extclass #'recur))])
         #'(lambda args
             (let recur ([stx stx-a])
               (syntax-parse stx
                 (~@ . macro-clauses)
                 (~@ . prod-clauses)
                 [_ (raise-syntax-error
                     #f
                     (string-append "not a " (#%datum . description))
                     this-syntax)])))))]))

(define (generate-prod-clause sspec-arg bspec-arg)
  (with-scope sc
    (define sspec (add-scope sspec-arg sc))
    (define bspec (if bspec-arg (add-scope bspec-arg sc) bspec-arg))
    
    (define sspec-pvars (sspec-bind-pvars! sspec))
    (with-syntax ([(v ...) (bound-id-set->list sspec-pvars)]
                  [pattern (compile-sspec-to-pattern sspec)]
                  [bspec-e (compile-bspec bspec sspec-pvars)]
                  [template (compile-sspec-to-template sspec)])
      #'[pattern
         (let* ([in (hash (~@ 'v (pattern-var-value v)) ...)]
                [out (simple-expand bspec-e in)])
           (rebind-pattern-vars
            (v ...)
            (values (hash-ref out 'v) ...)
            #'template))])))

(define (generate-macro-clause extclass recur-id)
  (let ([ext-info (lookup extclass extclass-rep?)])
    (when (not ext-info)
      (raise-syntax-error #f "not bound as extension class" extclass))
        
    (with-syntax ([m-pred (extclass-rep-pred ext-info)]
                  [m-acc (extclass-rep-acc ext-info)]
                  [recur recur-id])
      #'[(~or m:id (m:id . _))
         #:do [(define binding (lookup #'m m-pred))]
         #:when binding
         (recur (apply-as-transformer (m-acc binding)
                                      #'m
                                      'definition
                                      this-syntax))])))