#lang racket/base

(provide define-binding-class
         define-extension-class
         define-nonterminals
         define-nonterminal

         (for-syntax binding-class-predicate
                     binding-class-constructor
                     nonterminal-expander))
  
(require
  (for-syntax
   racket/base
   racket/list
   racket/syntax
   syntax/parse
   "syntax-classes.rkt"
   "../runtime/errors.rkt")
  (for-meta 2
            racket/base
            syntax/parse
            ee-lib
            "env-reps.rkt"
            "compile-nonterminal-expander.rkt"))

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
         #;(~optional (~seq #:define-literal-set set-name:id))
         prod:production-spec
         ...]
        ...)

     (let ([maybe-dup-id (check-duplicate-identifier
                          (filter (lambda (x) x)
                                  (flatten (attribute prod.form-name))))])
       (when maybe-dup-id
         (wrong-syntax maybe-dup-id "duplicate form name")))

     (define/syntax-parse (expander-name ...) (generate-temporaries #'(name ...)))
       
     #'(begin
         ; TODO improve message
         (begin
           (~? (define-syntax prod.form-name
                 (error-as-expression "may not be used as an expression"))
               (begin))
           ...)
         ...
         (begin-for-syntax
           #;(begin
               (~? (define-literal-set set-name (prod.form-name ...))
                   (begin))
               ...)
           (define-syntax name (nonterm-rep #'expander-name))
           ...
           (define expander-name
             (generate-nonterminal-expander
              #:allow-extension (~? extclass #f)
              #:description description
              prod ...))
           ...)
         )]))

(begin-for-syntax
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
    (accessor-macro nonterm-rep? "not bound as nonterminal" nonterm-rep-exp-proc)))