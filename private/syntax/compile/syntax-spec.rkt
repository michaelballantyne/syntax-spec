#lang racket/base

(provide sspec-bind-pvars!
         compile-sspec-to-pattern
         compile-sspec-to-template)

(require syntax/parse
         racket/syntax
         "../syntax-classes.rkt"
         "../env-reps.rkt"
         racket/pretty
         ee-lib
         syntax/id-set
         (only-in syntax/parse/private/residual-ct stxclass? has-stxclass-prop?)
         (for-template racket/base
                       syntax/parse))

(define (compile-sspec-to-pattern stx)
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
       #:do [(define binding (lookup #'r.ref (lambda (v) (nonterm-rep? v))))]
       #:when binding
       #'r.var]
      [r:ref-id
       #:do [(define binding (lookup #'r.ref (lambda (v) (or (stxclass? v)
                                                             (has-stxclass-prop? v)))))]
       #:when binding
       #'r.var]))
      
  (generate-template-form stx))

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
                                     (nonterm-rep? v)
                                     (stxclass? v)
                                     (has-stxclass-prop? v)))))
       (when (not binding)
         (wrong-syntax/orig #'r.ref "expected a reference to a binding class, syntax class, or nonterminal"))
       (when (member #'r.var res bound-identifier=?)
         (wrong-syntax/orig #'r.ref "duplicate pattern variable"))
       (bind! #'r.var (pvar-rep binding))
       
       (set! res (cons #'r.var res))]
      [_ (void)]))
  
  (reverse res))