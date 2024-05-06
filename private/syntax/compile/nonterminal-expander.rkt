#lang racket/base

(provide compile-nonterminal-expander generate-interface-expansion)
  
(require racket/base
         syntax/parse
         syntax/id-table
         racket/syntax
         ee-lib
         "../syntax-classes.rkt"
         "../env-reps.rkt"
         "syntax-spec.rkt"
         "binding-spec.rkt"

         (for-meta -2 ee-lib/private/lift-disappeareds)

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
       (with-syntax ([prod-clauses (generate-prod-clauses (attribute prod) maybe-nested-id (attribute variant) #'recur (attribute opts.space-stx) (attribute name))]
                     [macro-clauses (for/list ([extclass (attribute opts.ext-classes)])
                                      (generate-macro-clause extclass #'recur))]
                     [description (or (attribute opts.description) (symbol->string (syntax-e (attribute name))))]
                     [stx-a init-stx-id])
         #'(let recur ([stx stx-a])
             (let ([stx-dropped-props (lift-from-properties! stx)])
               (syntax-parse stx-dropped-props
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
            #`(wrap-hygiene
               (lambda (stx-a)
                 #,(generate-loop (add-scope #'(prod-arg ...) sc) id^ #'stx-a))
               'definition))]
         ;; Skip hygiene on the second pass of a exporting definition context to cooperate
         ;; with binder renaming. Exports will have been renamed on the first passs; if we
         ;; applied hygiene, we would improperly add inside-edge scopes to the renamed binders.
         ;;
         ;; All macro expansions, ~> rewritings should happen on the first pass so hygiene on
         ;; the second pass should be unneccessary.
         [#:pass2
          #`(lambda (stx-a)
              #,(generate-loop #'(prod-arg ...) #f #'stx-a))]
         [_
          #`(wrap-hygiene
             (lambda (stx-a)
               #,(generate-loop #'(prod-arg ...) #f #'stx-a))
             'definition)]))
     
     (generate-header)]))

(define (form-name prod)
  (syntax-parse prod
    [(p:production)
     (attribute p.form-name)]))

;; Given a sequence of productions beginning with a form production, return two lists:
;;  1. the prefix of form variants for the same form
;;  2. the remainder of the productions
;;
;; Example:
#; (list #'(a) #'(a e:expr) #'(b))
;; ->
#; (values (list #'(a) #'(a e:expr)) (list #'(b)))
;;
(define (gather-group prods)
  (let loop ([prods (cdr prods)]
             [group (list (car prods))])
    (if (and (pair? prods)
             (identifier? (form-name (car prods)))
             (bound-identifier=? (form-name (car group)) (form-name (car prods))))
        (loop (cdr prods) (cons (car prods) group))
        (values (reverse group) prods))))

;; Given a list of productions, return a list where sequential variants of forms are grouped
;; as a list. Raise an syntax when form variants are not adjacent.
;;
;; Example:
#; (list #'(a)
         #'[e:expr e:expr]
         #'(b)
         #'(b e:expr))
;; ->
#; (list (list #'(a))
         #'[e:expr e:expr]
         (list #'(b)
               #'(b e)))
;;
(define (group-form-productions prods)
  (let loop ([prods prods]
             [seen-forms (make-immutable-free-id-table)]
             [res '()])
    (if (null? prods)
        (reverse res)
        (syntax-parse (car prods)
          [(~or (p:form-production) (p:form-rewrite-production))
           (when (free-id-table-ref seen-forms #'p.form-name #f)
             (wrong-syntax/orig #'p.form-name "all variants of the same-named form must occur together"))
           (define-values (group remaining-prods) (gather-group prods))
           (loop remaining-prods (free-id-table-set seen-forms #'p.form-name #t) (cons group res))]
          [_ (loop (cdr prods) seen-forms (cons (car prods) res))]))))

(define (generate-prod-clauses prod-stxs maybe-nested-id variant recur-id binding-space-stx nt-name)
  (define grouped (group-form-productions prod-stxs))
  (for/list ([prod-or-group grouped])
    (generate-prod-group-clause prod-or-group maybe-nested-id variant recur-id binding-space-stx nt-name)))

(define (generate-prod-group-clause prod-group maybe-nested-id variant recur-id binding-space-stx nt-name)
  (if (syntax? prod-group)
      (syntax-parse prod-group
        [(p:rewrite-production)
         ;; Hygiene for rewrite productions only uses a macro introduction
         ;; scope applied to the input and flipped on the output. 
         (with-syntax ([recur recur-id])
           #`[p.pat
              p.parse-body ...
              (recur (syntax-track-origin
                      p.final-body
                      this-syntax
                      #'~>))])]
        [(p:syntax-production)
         (with-scope sc
           (define sspec (add-scope (attribute p.sspec) sc))
           (define maybe-bspec (and (attribute p.bspec) (add-scope (attribute p.bspec) sc)))

           (generate-prod-expansion
            sspec maybe-bspec (and maybe-nested-id (add-scope maybe-nested-id sc)) variant binding-space-stx
            (lambda () #`(syntax/loc this-syntax #,(compile-sspec-to-template sspec)))))])
      (syntax-parse (car prod-group)
        [(~or (p:form-production) (p:form-rewrite-production))
         #`[(~or #,(generate-pattern-literal #'p.form-name binding-space-stx)
                 (#,(generate-pattern-literal #'p.form-name binding-space-stx) . _))
            #,(generate-form-production-body prod-group maybe-nested-id variant recur-id binding-space-stx nt-name)]])))

(define (generate-form-production-body prod-group maybe-nested-id variant recur-id binding-space-stx nt-name)
  #`(syntax-parse this-syntax
      #,@(for/list ([prod prod-group])
           (syntax-parse prod
             [(p:form-production)
              (with-scope sc
                (define sspec (add-scope (attribute p.fspec) sc))
                (define maybe-bspec (and (attribute p.bspec) (add-scope (attribute p.bspec) sc)))

                (generate-prod-expansion
                 sspec maybe-bspec (and maybe-nested-id (add-scope maybe-nested-id sc)) variant binding-space-stx
                 (lambda ()
                   ;; Nasty workaround: the only syntax/lp we can define in userspace will misbehave given
                   ;; a template that just refers to a single pattern variable and put the this-syntax properties
                   ;; in place of those on the syntax in the pvar. So, detect that case and just use plain `syntax`.
                   (if (sspec-template-single-pvar? sspec)
                       #`(syntax #,(compile-sspec-to-template sspec))
                       #`(syntax-property (stx/lp this-syntax #,(compile-sspec-to-template sspec)) 'nonterminal '#,nt-name)))))]
             [(p:form-rewrite-production)
              ;; Hygiene for rewrite productions only uses a macro introduction
              ;; scope applied to the input and flipped on the output. 
              (with-syntax ([recur recur-id])
                #`[p.pat
                   p.parse-body ...
                   (recur (syntax-track-origin p.final-body
                                               this-syntax
                                               #'p.form-name))])]))))

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
         (recur (syntax-track-origin
                 (apply-as-transformer (m-acc binding)
                                       ((in-space 'm-space) #'m)
                                       'definition
                                       this-syntax)
                 this-syntax
                 #'m))])))
