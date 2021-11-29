#lang racket/base

(provide define-hosted-syntaxes
         (for-syntax binding-class-predicate
                     binding-class-constructor
                     nonterminal-expander
                     nonterminal-literal-set))
  
(require
  "../runtime/errors.rkt"
  (for-syntax
   racket/base
   racket/list
   racket/match
   racket/function
   racket/syntax
   syntax/parse
   ee-lib
   "syntax-classes.rkt"
   "../runtime/errors.rkt")
  ee-lib/define
  (for-meta 2
            racket/base
            syntax/parse
            racket/syntax
            ee-lib
            "env-reps.rkt"
            "syntax-classes.rkt"
            "compile/nonterminal-expander.rkt"))

(define-syntax define-hosted-syntaxes
  (syntax-parser
    [(_ form ...+)
     (match-define
       (list (list pass1-res expander-defs) ...)
       (for/list ([stx (attribute form)])
         (let-values ([(a b) (define-hosted-syntaxes-compile-form stx)])
           (list a b))))
     
     (with-syntax ([(pass1 ...) pass1-res]
                   [(expander-def ...) (filter identity expander-defs)])
       #'(begin
           pass1
           ...
           (begin-for-syntax
             expander-def
             ...)))]))

(begin-for-syntax
  ; syntax? -> (values syntax? (or/c syntax? #f))
  ; the first return value is used as part of the first-pass compilation;
  ; if present, the second value is used as part of the second-pass compilation,
  ; in begin-for-syntax.
  (define/hygienic (define-hosted-syntaxes-compile-form stx) #:expression
    (syntax-parse stx
      #:datum-literals (binding-class
                        extension-class
                        nonterminal nesting-nonterminal two-pass-nonterminal)
      
      [(binding-class name:id (~var descr (description #'name)))
       (with-syntax ([sname (format-id #'here "~a-var" #'name)]
                     [sname-pred (format-id #'here "~a-var?" #'name)])
         (values
          #'(begin-for-syntax
              (struct sname []
                #:property prop:procedure
                (dsl-error-as-expression (#%datum . descr.str)))
              (define-syntax name
                (bindclass-rep
                 (#%datum . descr.str)
                 (quote-syntax sname)
                 (quote-syntax sname-pred))))
          #f))]
      
      [(extension-class name:id (~var descr (description #'name)))
       (with-syntax ([sname (format-id #'here "~a" #'name)]
                     [sname-pred (format-id #'here "~a?" #'name)]
                     [sname-acc (format-id #'here "~a-transformer" #'name)])
         (values
          #'(begin-for-syntax
              (struct sname [transformer]
                #:property prop:procedure
                (dsl-error-as-expression (#%datum . descr.str)))
              (define-syntax name
                (extclass-rep (quote-syntax sname)
                              (quote-syntax sname-pred)
                              (quote-syntax sname-acc))))
          #f))]
      
      [(nonterminal
        name:id
        opts:nonterminal-options
        prod:production-spec ...+)
       (with-syntax ([expander-name (generate-temporary #'name)])
         (values
          (generate-nonterminal-declarations
           (attribute name) (attribute opts.description) (attribute prod.form-name)
           #'(simple-nonterm-info (quote-syntax expander-name)))
          #`(define expander-name
              (generate-nonterminal-expander
               #,this-syntax
               #:simple name opts prod ...))))]
      [(nesting-nonterminal
        name:id nested:nested-binding-syntax
        opts:nonterminal-options
        prod:production-spec ...+)
       (with-syntax ([expander-name (generate-temporary #'name)])
         (values
          (generate-nonterminal-declarations
           (attribute name) (attribute opts.description) (attribute prod.form-name)
           #'(nesting-nonterm-info (quote-syntax expander-name)))
          #`(define expander-name
              (generate-nonterminal-expander
               #,this-syntax
               (#:nesting nested.id) name opts prod ...))))]
      [(two-pass-nonterminal ~!
        name:id
        opts:nonterminal-options
        prod:production-spec ...+)
       (with-syntax ([(pass1-expander-name pass2-expander-name) (generate-temporaries #'(name name))])
         (values
          (generate-nonterminal-declarations
           (attribute name) (attribute opts.description) (attribute prod.form-name)
           #'(two-pass-nonterm-info (quote-syntax pass1-expander-name) (quote-syntax pass2-expander-name)))
          #`(begin
              (define pass1-expander-name
                (generate-nonterminal-expander
                 #,this-syntax
                 #:pass1 name opts prod ...))
              (define pass2-expander-name
                (generate-nonterminal-expander
                 #,this-syntax
                 #:pass2 name opts prod ...)))))])))

(begin-for-syntax
  (define (generate-nonterminal-declarations name-stx description form-names variant-info-stx)
    (with-syntax ([name name-stx]
                  [litset-name (generate-temporary name-stx)]
                  [error-message (make-error-message name-stx description)]
                  [(form-name ...) (filter identity form-names)]
                  [variant-info variant-info-stx])
      (check-duplicate-forms form-names)
      #'(begin
          (define-literal-forms litset-name 'error-message (form-name ...))
          (begin-for-syntax
            (define-syntax name (nonterm-rep (quote-syntax litset-name) variant-info))))))
  
  (define (check-duplicate-forms form-names)
    (let ([maybe-dup-id (check-duplicate-identifier
                         (filter identity (flatten form-names)))])
      (when maybe-dup-id
        (wrong-syntax maybe-dup-id "duplicate form name"))))

  (define (make-error-message name description)
    (string-append
     (if description (syntax-e description) (symbol->string (syntax-e name)))
     " may not be used as an expression"))
  
  (define-syntax generate-nonterminal-expander
    (syntax-parser
      [(_ orig-stx . decls)
       (parameterize ([current-orig-stx #'orig-stx])
         (compile-nonterminal-expander #'decls))]))  
  )

(define-syntax define-nonterminal
  (syntax-parser
    [(_ . spec)
     #'(define-nonterminals
         spec)]))

(begin-for-syntax
  (begin-for-syntax
    (define (accessor-macro predicate error-message accessor)
      (syntax-parser
        [(_ ref:id)
         (define binding (lookup #'ref predicate))
         (when (not binding)
           (wrong-syntax #'ref error-message))
         (accessor binding)])))

  (define-syntax nonterminal-expander
    (syntax-parser
      [(_ ref:id)
       (define binding (lookup #'ref nonterm-rep?))
       (when (not binding)
         (wrong-syntax #'ref  "expected a nonterminal name"))
       (define variant-info (nonterm-rep-variant-info binding))
       (when (not (simple-nonterm-info? variant-info))
         (wrong-syntax #'ref "only simple non-terminals may be used as entry points"))
       (simple-nonterm-info-expander variant-info)]))
      
  (define-syntax binding-class-constructor
    (accessor-macro bindclass-rep? "expected a binding class name" bindclass-rep-constr))
  
  (define-syntax binding-class-predicate
    (accessor-macro bindclass-rep? "expected a binding class name" bindclass-rep-pred))

  (define-syntax nonterminal-literal-set
    (accessor-macro nonterm-rep? "expected a nonterminal name" nonterm-rep-litset-ref)))