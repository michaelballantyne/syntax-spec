#lang racket/base

(provide define-binding-class
         define-extension-class
         define-nonterminals
         define-nonterminal

         (for-syntax binding-class-predicate
                     binding-class-constructor
                     nonterminal-expander
                     nonterminal-literal-set))
  
(require
  (for-syntax
   racket/base
   racket/list
   racket/function
   racket/syntax
   syntax/parse
   "syntax-classes.rkt")
  ee-lib/define
  (for-meta 2
            racket/base
            syntax/parse
            ee-lib
            "env-reps.rkt"
            "compile/nonterminal-expander.rkt"))

(define-syntax define-binding-class
  (syntax-parser
    [(_ name:id description:expr)
     (with-syntax ([sname (format-id #'here "~a-var" #'name)]
                   [sname-pred (format-id #'here "~a-var?" #'name)])
       #'(begin-for-syntax
           (struct sname [])
           (define-syntax name
             (bindclass-rep
              (#%datum . description)
              (quote-syntax sname)
              (quote-syntax sname-pred)))))]))

(define-syntax define-extension-class
  (syntax-parser
    [(_ name:id)
     (with-syntax ([sname (format-id #'here "~a" #'name)]
                   [sname-pred (format-id #'here "~a?" #'name)]
                   [sname-acc (format-id #'here "~a-transformer" #'name)])
       #'(begin-for-syntax
           (struct sname [transformer])
           (define-syntax name
             (extclass-rep (quote-syntax sname)
                           (quote-syntax sname-pred)
                           (quote-syntax sname-acc)))))]))
  
(define-syntax define-nonterminals
  (syntax-parser
    [(_ [name:id
         #:description description:string
         (~optional (~seq #:allow-extension extclass:id))
         prod:production-spec
         ...]
        ...)

     (check-duplicate-forms (attribute prod.form-name))

     (with-syntax ([((form-name ...) ...) (for/list ([prod-forms (attribute prod.form-name)])
                                            (filter identity prod-forms))]
                   [(expander-name ...) (generate-temporaries #'(name ...))]
                   [(litset-name ...) (generate-temporaries #'(name ...))]
                   [(error-message ...) (map make-error-message (attribute description))])
       #'(begin
           (define-literal-forms litset-name 'error-message (form-name ...))
           ...
           
           (begin-for-syntax
             (define-syntax name (nonterm-rep #'expander-name #'litset-name))
             ...
             
             (define expander-name
               (generate-nonterminal-expander
                #:allow-extension (~? extclass #f)
                #:description description
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