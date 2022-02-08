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
     (define (generate-loop prods-stx maybe-nested-id init-stx-id maybe-nest-st-id)
       (define/syntax-parse ((prod:production) ...) prods-stx)
       (with-syntax ([prod-clauses (map (lambda (prod)
                                          (generate-prod-clause prod maybe-nested-id maybe-nest-st-id (attribute variant) #'recur (attribute opts.space-stx)))
                                        (attribute prod))]
                     [macro-clauses (for/list ([extclass (attribute opts.ext-classes)])
                                      (generate-macro-clause extclass #'recur))]
                     [description (or (attribute opts.description) (symbol->string (syntax-e (attribute name))))]
                     [stx-a init-stx-id])
         #'(parameterize
               ;; TODO: find a cleaner way to name the interface macro for syntax production errors.
               ([current-orig-stx (or (current-orig-stx) (current-syntax-context))])
             (let recur ([stx stx-a])
               (syntax-parse stx
                 (~@ . macro-clauses)
                 (~@ . prod-clauses)
                 [_ (wrong-syntax/orig
                     this-syntax
                     (string-append "expected " (#%datum . description)))])))))

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

(define (generate-prod-clause prod-stx maybe-nested-id maybe-nest-st-id variant recur-id binding-space-stx)
  (syntax-parse prod-stx
    [(p:rewrite-production)
     (with-syntax ([recur recur-id])
       #`[p.pat
          (recur (let () p.body ...))])]
    [(p:syntax-production)
     (with-scope sc
       (define sspec (add-scope (attribute p.sspec) sc))
       (define maybe-bspec (and (attribute p.bspec) (add-scope (attribute p.bspec) sc)))
    
       (define sspec-pvars (sspec-bind-pvars! sspec))
       (define bound-pvars (if maybe-nested-id
                               (append sspec-pvars (list (add-scope maybe-nested-id sc)))
                               sspec-pvars))
    
       (with-syntax ([(v ...) sspec-pvars]
                     [pattern (compile-sspec-to-pattern sspec binding-space-stx)]
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
                     #'#'template)))]))]))

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