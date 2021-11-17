#lang racket/base

(provide compile-nonterminal-expander)
  
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
                       "../../runtime/binding-spec.rkt"
                       "pattern-var-reflection.rkt"
                       syntax/parse
                       ee-lib))

(define (compile-nonterminal-expander stx)
  (syntax-parse stx
    #:context 'compile-nonterminal-expander
    [(variant
      name
      (opts:nonterminal-options)
      prod-arg ...)
     (define (generate-loop prods-stx maybe-nested-id init-stx-id maybe-nest-st-id)
       (define/syntax-parse ((prod:production-spec) ...) prods-stx)
       (with-syntax ([prod-clauses (map (lambda (sspec bspec)
                                          (generate-prod-clause sspec bspec maybe-nested-id maybe-nest-st-id (attribute variant)))
                                        (attribute prod.sspec) (attribute prod.bspec))]
                     [macro-clauses (for/list ([extclass (attribute opts.ext-classes)])
                                      (generate-macro-clause extclass #'recur))]
                     [description (or (attribute opts.description) (symbol->string (syntax-e (attribute name))))]
                     [stx-a init-stx-id])
         #'(let recur ([stx stx-a])
             (syntax-parse stx
               (~@ . macro-clauses)
               (~@ . prod-clauses)
               [_ (raise-syntax-error
                   #f
                   (string-append "expected " (#%datum . description))
                   this-syntax)]))))

     (define (generate-header)
       (syntax-parse (attribute variant)
         [(#:nesting nested-id:id)
          (with-scope sc
            (define id^ (bind! (add-scope (attribute nested-id) sc) (pvar-rep (nested-binding))))
            #`(lambda (stx-a nest-st)
                #,(generate-loop (add-scope #'(prod-arg ...) sc) id^ #'stx-a #'nest-st)))]
         [_
          #`(lambda (stx-a)
              #,(generate-loop #'(prod-arg ...) #f #'stx-a #f))]))
     
     (generate-header)]))

(define (generate-prod-clause sspec-arg maybe-bspec-arg maybe-nested-id maybe-nest-st-id variant)
  (with-scope sc
    (define sspec (add-scope sspec-arg sc))
    (define maybe-bspec (if maybe-bspec-arg (add-scope maybe-bspec-arg sc) maybe-bspec-arg))
    
    (define sspec-pvars (sspec-bind-pvars! sspec))
    (define bound-pvars (if maybe-nested-id
                            (append sspec-pvars (list (add-scope maybe-nested-id sc)))
                            sspec-pvars))
    
    (with-syntax ([(v ...) sspec-pvars]
                  [pattern (compile-sspec-to-pattern sspec)]
                  [bspec-e (compile-bspec maybe-bspec bound-pvars variant)]
                  [template (compile-sspec-to-template sspec)]
                  [nest-st (or maybe-nest-st-id #'#f)])
      #`[pattern
         (let*-values ([(in) (hash (~@ 'v (pattern-var-value v)) ...)]
                       [(out nest-st^) (simple-expand bspec-e in nest-st)])
           (rebind-pattern-vars
            (v ...)
            (values (hash-ref out 'v) ...)
            #,(if maybe-nest-st-id
                  #'(values #'template nest-st^)
                  #'#'template)))])))

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