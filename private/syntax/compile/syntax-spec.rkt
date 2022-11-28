#lang racket/base

(provide sspec-bind-pvars!
         compile-sspec-to-pattern
         compile-sspec-to-template
         generate-pattern-literal
         sspec-template-composite?)

(require racket/function
         syntax/parse
         "../syntax-classes.rkt"
         "../env-reps.rkt"
         ee-lib
         (only-in syntax/parse/private/residual-ct stxclass? has-stxclass-prop?)
         (for-template racket/base
                       syntax/parse
                       "../../runtime/syntax-classes.rkt"))

(define (generate-pattern-literal name-stx binding-space-stx)
  (with-syntax ([name name-stx]
                [binding-space binding-space-stx])
    #'(~var _ (literal-in-space (quote-syntax name) (quote binding-space)))))

(define (compile-sspec-to-pattern stx binding-space-stx)
  (define generate-pattern-form
    (syntax-parser
      #:context 'generate-pattern-form
      [(name:form-id . term)
       (with-syntax ([literal-c (generate-pattern-literal #'name binding-space-stx)]
                     [term-c (generate-pattern-term #'term)])
         #'(literal-c . term-c))]
      [name:form-id
       (generate-pattern-literal #'name binding-space-stx)]
      [_ (generate-pattern-term this-syntax)]))
  
  (define generate-pattern-term
    (syntax-parser
      #:context 'generate-pattern-term
      [()
       #'()]
      [k:keyword
       this-syntax]
      [(~datum ...)
       (syntax/loc this-syntax (... ...))]
      [(~datum ...+)
       (syntax/loc this-syntax (... ...+))]
      [((~datum ~literal) lit:id (~optional (~seq #:space space:id) #:defaults ([space #'#f])))
       (generate-pattern-literal #'lit #'space)]
      [((~datum ~datum) datum)
       #'(~datum datum)]
      [(t1 . t2)
       (with-syntax ([t1-c (generate-pattern-term #'t1)]
                     [t2-c (generate-pattern-term #'t2)])
         #'(t1-c . t2-c))]
      [r:ref-id
       #:do [(define binding (lookup #'r.ref (disjoin bindclass-rep? extclass-rep?)))]
       #:when binding
       #'(~var r.var id)]
      [r:ref-id
       #:do [(define binding (lookup #'r.ref (lambda (v) (nonterm-rep? v))))]
       #:when binding
       #'r.var]
      [r:ref-id
       #:do [(define binding (lookup #'r.ref (lambda (v) (or (stxclass? v)
                                                             (has-stxclass-prop? v)))))]
       #:when binding
       #'(~var r.var r.ref)]
      [_ (wrong-syntax/orig this-syntax "expected a syntax spec term")]))
      
  (generate-pattern-form stx))

(define (compile-sspec-to-template stx)
  (define generate-template-form
    (syntax-parser
      #:context 'generate-pattern-form
      [(name:form-id . term)
       (with-syntax ([term-c (generate-template-term #'term)])
         #'(name . term-c))]
      [name:form-id
       #'name]
      [_ (generate-template-term this-syntax)]))
  
  (define generate-template-term
    (syntax-parser
      #:context 'generate-template-term
      [()
       #'()]
      [k:keyword
       this-syntax]
      [(~datum ...)
       #'(... ...)]
      [(~datum ...+)
       #'(... ...)]
      [((~datum ~literal) lit:id (~optional (~seq #:space space:id) #:defaults ([space #'#f])))
       #'(... (... lit))]
      [((~datum ~datum) datum)
       #'(... (... datum))]
      [(t1 . t2)
       (with-syntax ([t1-c (generate-template-term #'t1)]
                     [t2-c (generate-template-term #'t2)])
         #'(t1-c . t2-c))]
      [r:ref-id
       #:do [(define binding (lookup #'r.ref (disjoin bindclass-rep? extclass-rep?)))]
       #:when binding
       #'r.var]
      [r:ref-id
       #:do [(define binding (lookup #'r.ref (lambda (v) (nonterm-rep? v))))]
       #:when binding
       #'r.var]
      [r:ref-id
       #:do [(define binding (lookup #'r.ref (lambda (v) (or (stxclass? v)
                                                             (has-stxclass-prop? v)))))]
       #:when binding
       #'r.var]))
      
  (generate-template-form stx))

(define (sspec-template-composite? stx)
  (syntax-parse stx
    [name:form-id
     #f]
    [r:ref-id
     #f]
    [_ #t]))
    

(define (sspec-bind-pvars! stx)
  (define res '())

  ;; records left-to-right
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
                                     (extclass-rep? v)
                                     (nonterm-rep? v)
                                     (stxclass-rep? v)))))
       (when (not binding)
         (wrong-syntax/orig #'r.ref "expected a reference to a binding class, extension class, syntax class, or nonterminal"))
       (when (member #'r.var res bound-identifier=?)
         (wrong-syntax/orig #'r.ref "duplicate pattern variable"))
       (bind! #'r.var (pvar-rep binding))
       
       (set! res (cons #'r.var res))]
      [_ (void)]))
  
  (reverse res))
