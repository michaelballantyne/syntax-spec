#lang racket/base

(provide define-hosted-syntaxes
         (for-syntax binding-class-predicate
                     binding-class-constructor
                     nonterminal-expander
                     nonterminal-literal-set))
  
(require
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
            ee-lib
            "env-reps.rkt"
            "compile/nonterminal-expander.rkt"))

(define-syntax define-hosted-syntaxes
  (syntax-parser
    #:context 'define-hosted-syntaxes
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
  (define/hygienic (define-hosted-syntaxes-compile-form stx) #:expression
    (syntax-parse stx
      #:context 'define-hosted-syntaxes-pass1
      #:datum-literals (binding-class extension-class nonterminal)
      
      [(binding-class name:id description:expr)
       (with-syntax ([sname (format-id #'here "~a-var" #'name)]
                     [sname-pred (format-id #'here "~a-var?" #'name)])
         (values
          #'(begin-for-syntax
              (struct sname [])
              (define-syntax name
                (bindclass-rep
                 (#%datum . description)
                 (quote-syntax sname)
                 (quote-syntax sname-pred))))
          #f))]
      
      [(extension-class name:id)
       (with-syntax ([sname (format-id #'here "~a" #'name)]
                     [sname-pred (format-id #'here "~a?" #'name)]
                     [sname-acc (format-id #'here "~a-transformer" #'name)])
         (values
          #'(begin-for-syntax
              (struct sname [transformer])
              (define-syntax name
                (extclass-rep (quote-syntax sname)
                              (quote-syntax sname-pred)
                              (quote-syntax sname-acc))))
          #f))]

      [(nonterminal name:id (~optional (nested-id:id))
                    #:description description:string
                    (~optional (~seq #:allow-extension ext:extclass-spec))
                    prod:production-spec ...)

       (check-duplicate-forms (attribute prod.form-name))

       (with-syntax ([litset-name (generate-temporary #'name)]
                     [error-message (make-error-message (attribute description))]
                     [(form-name ...) (filter identity (attribute prod.form-name))]
                     [rep-to-use (if (attribute nested-id) #'sequence-nonterm-rep #'nonterm-rep)]
                     [expander-name (generate-temporary #'name)])
         (values
          #'(begin
              (define-literal-forms litset-name 'error-message (form-name ...))
              (begin-for-syntax
                (define-syntax name (rep-to-use #'expander-name #'litset-name))))
          #'(define expander-name
              (generate-nonterminal-expander
               #:description description
               #:allow-extension (~? (ext.classes ...) ())
               #:nested-id (~? nested-id #f)
               prod ...))))]))
  )
  
#;(define-syntax define-nonterminals
    (syntax-parser
      [(_ [name:id (~optional (nested-id:id))
                   #:description description:string
                   (~optional (~seq #:allow-extension ext:extclass-spec))
                   prod:production-spec
                   ...]
          ...)

       (check-duplicate-forms (attribute prod.form-name))

       (with-syntax ([((form-name ...) ...) (for/list ([prod-forms (attribute prod.form-name)])
                                              (filter identity prod-forms))]
                   
                     [(litset-name ...) (generate-temporaries #'(name ...))]
                     [(error-message ...) (map make-error-message (attribute description))]
                     [(rep-to-use ...) (map (lambda (nested-id)
                                              (if nested-id #'sequence-nonterm-rep #'nonterm-rep))
                                            (attribute nested-id))])
         #'(begin
             (define-literal-forms litset-name 'error-message (form-name ...))
             ...
           
             (begin-for-syntax
               (define-syntax name (rep-to-use #'expander-name #'litset-name))
               ...
             
               (define expander-name
                 (generate-nonterminal-expander
                  #:description description
                  #:allow-extension (~? (ext.classes ...) ())
                  #:nested-id (~? nested-id #f)
                  prod ...))
               ...)
             ))]))

(begin-for-syntax
  (define (check-duplicate-forms form-names)
    (let ([maybe-dup-id (check-duplicate-identifier
                         (filter identity (flatten form-names)))])
      (when maybe-dup-id
        (wrong-syntax maybe-dup-id "duplicate form name"))))

  (define (make-error-message description)
    (string-append
     (syntax-e description)
     " may not be used as an expression"))
  
  (define-syntax generate-nonterminal-expander
    (syntax-parser [(_ . decls) (compile-nonterminal-expander #'decls)])))

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
           (raise-syntax-error #f error-message #'ref))
         (accessor binding)])))
      
  (define-syntax binding-class-constructor
    (accessor-macro bindclass-rep? "not bound as binding class" bindclass-rep-constr))
  
  (define-syntax binding-class-predicate
    (accessor-macro bindclass-rep? "not bound as binding class" bindclass-rep-pred))

  (define-syntax nonterminal-expander
    (accessor-macro nonterm-rep? "not bound as nonterminal" nonterm-rep-exp-proc))

  (define-syntax nonterminal-literal-set
    (accessor-macro nonterm-rep? "not bound as nonterminal" nonterm-rep-litset-ref)))