#lang racket/base

(provide define-hosted-syntaxes
         define-host-interface/expression
         define-host-interface/definition
         define-host-interface/definitions
         
         (for-syntax binding-class-predicate
                     binding-class-constructor
                     nonterminal-expander))
  
(require
  "../runtime/errors.rkt"
  "../runtime/compile.rkt"
  ee-lib/define
  
  (for-syntax
   racket/base
   racket/list
   racket/match
   racket/function
   racket/syntax
   syntax/parse
   syntax/id-set
   ee-lib
   ee-lib/persistent-id-table
   "syntax-classes.rkt"
   "../runtime/binding-spec.rkt"
   "../runtime/errors.rkt"
   ee-lib/private/lift-trampoline)
  
  (for-meta 2
            racket/base
            syntax/parse
            racket/syntax
            ee-lib
            "env-reps.rkt"
            "syntax-classes.rkt"
            "compile/syntax-spec.rkt"
            "compile/nonterminal-expander.rkt"))

;;
;; define-hosted-syntaxes
;;

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
      
      [(binding-class name:id
                      (~var descr (maybe-description #'name))
                      (~var space maybe-binding-space))
       (with-syntax ([sname (format-id #'here "~a-var" #'name)]
                     [sname-pred (format-id #'here "~a-var?" #'name)])
         (values
          #'(begin-for-syntax
              (struct sname []
                #:property prop:procedure
                (binding-as-rkt (quote-syntax name) (#%datum . descr.str)))
              (define-syntax name
                (bindclass-rep
                 (#%datum . descr.str)
                 (quote-syntax sname)
                 (quote-syntax sname-pred)
                 (quote space.stx))))
          #f))]
      
      [(extension-class name:id
                        (~var descr (maybe-description #'name))
                        (~var space maybe-binding-space))
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
                              (quote-syntax sname-acc)
                              (quote space.stx))))
          #f))]
      
      [(nonterminal
         name:id
         opts:nonterminal-options
         prod:production ...+)
       (with-syntax ([expander-name (generate-temporary #'name)])
         (values
          (generate-nonterminal-declarations
           (attribute name) (attribute opts) (attribute prod.form-name)
           #'(simple-nonterm-info (quote-syntax expander-name)))
          #`(define expander-name
              (generate-nonterminal-expander
               #,this-syntax
               #:simple name opts prod ...))))]
      [(nesting-nonterminal
         name:id nested:nested-binding-syntax
         opts:nonterminal-options
         prod:production ...+)
       (with-syntax ([expander-name (generate-temporary #'name)])
         (values
          (generate-nonterminal-declarations
           (attribute name) (attribute opts) (attribute prod.form-name)
           #'(nesting-nonterm-info (quote-syntax expander-name)))
          #`(define expander-name
              (generate-nonterminal-expander
               #,this-syntax
               (#:nesting nested.id) name opts prod ...))))]
      [(two-pass-nonterminal ~!
         name:id
         opts:nonterminal-options
         prod:production ...+)
       (with-syntax ([(pass1-expander-name pass2-expander-name) (generate-temporaries #'(name name))])
         (values
          (generate-nonterminal-declarations
           (attribute name) (attribute opts) (attribute prod.form-name)
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
  (define (generate-nonterminal-declarations name-stx opts-stx form-names variant-info-stx)
    (define/syntax-parse (opts:nonterminal-options) opts-stx)
    (with-syntax ([name name-stx]
                  [litset-name (or (attribute opts.litset-binder) (generate-temporary name-stx))]
                  [error-message (make-error-message name-stx (attribute opts.description))]
                  [(form-name ...) (deduplicate-form-names (filter identity form-names))]
                  [variant-info variant-info-stx])
      #'(begin
          (define-literal-forms litset-name #:binding-space opts.space-stx 'error-message (form-name ...))
          (begin-for-syntax
            (define-syntax name (nonterm-rep variant-info))))))

  (define (deduplicate-form-names form-names)
    (bound-id-set->list (immutable-bound-id-set form-names)))

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

;;
;; interface macro definition forms
;;


(define-syntax define-host-interface/expression
  (syntax-parser
    [(_ (name:id . sspec)
        (~optional (~seq #:binding bspec))
        parse-body ...)
     #'(define-syntax name
         (expression-macro
          (generate-host-interface-transformer
           name sspec (~? (bspec) ()) (#:simple) parse-body ...)))]))

(define-syntax define-host-interface/definitions
  (syntax-parser
    [(_ (name:id . sspec)
        (~optional (~seq #:binding bspec))
        parse-body ...)
     #'(define-syntax name
         (definition-macro
           (wrap-bind-trampoline
            (wrap-persist
             (generate-host-interface-transformer
              name sspec (~? (bspec) ()) (#:pass1 #:pass2) parse-body ...)))))]))

(begin-for-syntax
  (define-syntax generate-host-interface-transformer
    (syntax-parser
      [(_ ctx-id sspec ((~optional (~seq bspec))) (variants ...) parse-body ...)
       (with-scope sc
         (define (generate-body)
           (add-scope
            #'(syntax-parse #f
                [_
                 parse-body ...])
            sc))

         (define/syntax-parse clause
           (generate-interface-expansion
            (add-scope (attribute sspec) sc)
            (and (attribute bspec) (add-scope (attribute bspec) sc))
            (syntax->list #'(variants ...))
            generate-body))

         #'(syntax-parser
             [(_ . rest)
              (define ctx this-syntax)
              (syntax-parse (attribute rest)
                #:context #'ctx-id
                clause)]))])))


(define-syntax define-host-interface/definition
  (syntax-parser
    #:datum-literals (-> define)
    [(_ (name:id . sspec)
        (~optional (~seq #:binding bspec))
        -> (define [name-parse-body ...] [rhs-parse-body ...]))
     #'(begin
         (define-syntax pass2-macro
           (expression-macro
            (generate-host-interface-transformer
             name sspec (~? (bspec) ()) (#:pass2) rhs-parse-body ...)))
         (define-syntax name
           (definition-macro
             (wrap-bind-trampoline
              (wrap-persist
               (generate-host-interface-transformer/definition-pass1
                sspec (~? (bspec) ()) [name-parse-body ...] pass2-macro))))))]))

(begin-for-syntax
  (define-syntax generate-host-interface-transformer/definition-pass1
    (syntax-parser
      [(_ sspec-arg ((~optional (~seq bspec-arg))) [name-parse-body ...] pass2-macro)
       (with-scope sc
         (define sspec (add-scope (attribute sspec-arg) sc))
         (define bspec (and (attribute bspec-arg) (add-scope (attribute bspec-arg) sc)))
         
         (define (generate-body)
           #`(with-syntax ([compiled-name (syntax-parse #f
                                            [_
                                             #,@(add-scope #'[name-parse-body ...] sc)])])
               (trampoline-lift! #'(define compiled-name (pass2-macro . #,(compile-sspec-to-template sspec))))
               #'(begin)))

         (define/syntax-parse clause
           (generate-interface-expansion
            sspec
            bspec
            (list #'#:pass1)
            generate-body))

         #'(syntax-parser
             [(_ . rest)
              (define ctx this-syntax)
              (syntax-parse (attribute rest)
                #:context ctx
                clause)]))])))

;;
;; phase 1 accessors
;;

(begin-for-syntax
  (define-syntax nonterminal-expander
    (syntax-parser
      [(_ ref:id)
       (define binding (lookup #'ref nonterm-rep?))
       (when (not binding)
         (wrong-syntax #'ref  "expected a nonterminal name"))
       (define variant-info (nonterm-rep-variant-info binding))
       (when (not (simple-nonterm-info? variant-info))
         (wrong-syntax #'ref "only simple non-terminals may be used as entry points"))
       #`(lambda (stx)
           (simple-expand-single-exp
            #,(simple-nonterm-info-expander variant-info)
            stx))]))
  
  (begin-for-syntax
    (define (accessor-macro predicate error-message accessor)
      (syntax-parser
        [(_ ref:id)
         (define binding (lookup #'ref predicate))
         (when (not binding)
           (wrong-syntax #'ref error-message))
         (accessor binding)])))
      
  (define-syntax binding-class-constructor
    (accessor-macro bindclass-rep? "expected a binding class name" bindclass-rep-constr))
  
  (define-syntax binding-class-predicate
    (accessor-macro bindclass-rep? "expected a binding class name" bindclass-rep-pred))
  )