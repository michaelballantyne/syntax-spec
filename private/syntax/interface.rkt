#lang racket/base

(provide syntax-spec
         (for-syntax racket-expr
                     racket-var
                     racket-macro
                     
                     binding-class-predicate
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
   syntax/id-table
   ee-lib
   ee-lib/persistent-id-table
   ee-lib/syntax-category
   "syntax-classes.rkt"
   "../runtime/binding-spec.rkt"
   "../runtime/errors.rkt"
   ee-lib/private/lift-trampoline)
  
  (for-meta 2
            racket/base
            syntax/parse
            racket/syntax
            racket/match
            ee-lib
            "env-reps.rkt"
            "syntax-classes.rkt"
            "compile/syntax-spec.rkt"
            "compile/nonterminal-expander.rkt"))

;;
;; syntax-spec
;;

(define-syntax syntax-spec
  (module-macro
   (syntax-parser
     [(_ form ...+)
      (match-define
        (list (list pass1-res pass2-res pass3-res) ...)
        (for/list ([stx (attribute form)])
          (let-values ([(a b c) (syntax-spec-compile-form stx)])
            (list a b c))))
     
      (with-syntax ([(pass1 ...) (filter identity pass1-res)]
                    [(pass2 ...) (filter identity pass2-res)]
                    [(pass3 ...) (filter identity pass3-res)])
        #'(begin
            pass1
            ...
            (begin-for-syntax
              pass2
              ...)
            pass3
            ...))])))

(begin-for-syntax
  ;; syntax? -> (values (or/c syntax? #f) (or/c syntax? #f) (or/c syntax? #f))
  ;; Three return values for three passes of expansion of the generated code.
  ;;
  ;; pass 1: define-syntax declarations for binding and extension classes and nonterminals.
  ;; pass 2: nonterminal expander definitions; these all get added to a single begin-for-syntax
  ;; pass 3: host interface macro definitions.
  ;;
  ;; Racket only applies its trick of allowing mutual recursion between begin-for-syntaxes
  ;; by allowing forward references apparently only applies to identifiers with module scope.
  ;; Here we're using generated names, so we can't have any forward references; all recursion
  ;; happens within the single begin-for-syntax.
  (define/hygienic (syntax-spec-compile-form stx) #:expression
    (syntax-parse stx
      #:datum-literals (binding-class
                        extension-class
                        nonterminal nonterminal/nesting nonterminal/two-pass
                        host-interface/expression
                        host-interface/definition
                        host-interface/definitions)
      
      [(binding-class
        ~! name:id
        (~var descr (maybe-description #'name))
        (~var space maybe-binding-space))
       (with-syntax ([sname (format-id #'here "~a-var" #'name)]
                     [sname-pred (format-id #'here "~a-var?" #'name)])
         (values
          #'(begin-for-syntax
              (struct sname []
                #:property prop:set!-transformer
                (binding-as-rkt (quote-syntax name) (#%datum . descr.str))
                #:property prop:not-racket-syntax #t)
              (define-syntax name
                (bindclass-rep
                 (#%datum . descr.str)
                 (quote-syntax sname)
                 (quote-syntax sname-pred)
                 (quote space.stx))))
          #f
          #f))]
      
      [(extension-class
        ~! name:id
        (~var descr (maybe-description #'name))
        (~var space maybe-binding-space))
       (with-syntax ([sname (format-id #'here "~a" #'name)]
                     [sname-pred (format-id #'here "~a?" #'name)]
                     [sname-acc (format-id #'here "~a-transformer" #'name)])
         (values
          #'(begin-for-syntax
              (struct sname [transformer]
                #:property prop:procedure
                (dsl-error-as-expression (#%datum . descr.str))
                #:property prop:not-racket-syntax #t)
              (define-syntax name
                (extclass-rep (quote-syntax sname)
                              (quote-syntax sname-pred)
                              (quote-syntax sname-acc)
                              (quote space.stx))))
          #f
          #f))]
      
      [(nonterminal ~!
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
               #:simple name opts prod ...))
          #f))]
      [(nonterminal/nesting ~!
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
               (#:nesting nested.id) name opts prod ...))
          #f))]
      [(nonterminal/two-pass ~!
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
                 #:pass2 name opts prod ...)))
          #f))]
      [(host-interface/expression
         ~! (name:id . sspec)
         (~optional (~seq #:binding bspec))
         parse-body ...+)
       (values
        #f
        #f
        #'(define-syntax name
            (expression-macro
             (generate-host-interface-transformer
              name sspec (~? (bspec) ()) (#:simple) parse-body ...))))]
      [(host-interface/definitions
         ~! (name:id . sspec)
         (~optional (~seq #:binding bspec))
         parse-body ...+)
       (values
        #f
        #f
        #'(define-syntax name
            (definition-macro
              (wrap-bind-trampoline
               (wrap-persist
                (generate-host-interface-transformer
                 name sspec (~? (bspec) ()) (#:pass1 #:pass2) parse-body ...))))))]
      [(host-interface/definition
         ~! (name:id . sspec)
         (~optional (~seq #:binding bspec))
         #:lhs [name-parse-body ...+]
         #:rhs [rhs-parse-body ...+])
       (values
        #f
        #f
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
                   sspec (~? (bspec) ()) [name-parse-body ...] pass2-macro)))))))])))

(begin-for-syntax
  (define (generate-nonterminal-declarations name-stx opts-stx form-names variant-info-stx)
    (define/syntax-parse (opts:nonterminal-options) opts-stx)
    (with-syntax ([name name-stx]
                  [litset-name (or (attribute opts.litset-binder) (generate-temporary name-stx))]
                  [error-message (make-error-message name-stx (attribute opts.description))]
                  [(form-name ...) (deduplicate-form-names (filter identity form-names))]
                  [variant-info variant-info-stx])
      #'(begin
          (define-literal-forms litset-name #:binding-space opts.space-stx error-message (form-name ...))
          (begin-for-syntax
            (define-syntax name (nonterm-rep variant-info))))))

  (define (deduplicate-form-names form-names)
    (remove-duplicates form-names bound-identifier=?))

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

(begin-for-syntax
  (define-syntax generate-host-interface-transformer
    (syntax-parser
      [(_ ctx-id sspec ((~optional (~seq bspec))) (variants ...) parse-body ...+)
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

(begin-for-syntax
  (define (check-lhs-result stx)
    (syntax-parse stx
      [name:id #'(name)]
      [(name:id ...) this-syntax]))

  (define-syntax generate-host-interface-transformer/definition-pass1
    (syntax-parser
      [(_ sspec-arg ((~optional (~seq bspec-arg))) [name-parse-body ...+] pass2-macro)
       (with-scope sc
         (define sspec (add-scope (attribute sspec-arg) sc))
         (define bspec (and (attribute bspec-arg) (add-scope (attribute bspec-arg) sc)))
         
         (define (generate-body)
           #`(with-syntax ([compiled-names (check-lhs-result
                                           (syntax-parse #f
                                             [_
                                              #,@(add-scope #'[name-parse-body ...] sc)]))])
               (trampoline-lift! #'(define-values compiled-names (pass2-macro . #,(compile-sspec-to-template sspec))))
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

(begin-for-syntax
  (define racket-macro-constructor (lambda (x) x))
  (define racket-macro? (lambda (v) (and (or (set!-transformer? v) (procedure? v))
                                         (not (not-racket-syntax? v)))))
  (define racket-macro-transformer (lambda (v)
                                     (if (set!-transformer? v)
                                         (set!-transformer-procedure v)
                                         v)))
  (define-syntax racket-macro
    (extclass-rep (quote-syntax racket-macro-constructor)
                  (quote-syntax racket-macro?)
                  (quote-syntax racket-macro-transformer)
                  #f)))

;; racket var is not super-special.
;; It's just a binding class that gets an implicit
;; (with-reference-compilers ([racket-var mutable-reference-compiler])).
(syntax-spec
  (binding-class racket-var #:description "racket variable"))

(begin-for-syntax
  (define built-in-reference-compilers (list (list #'racket-var mutable-reference-compiler)))
  (setup-default-reference-compilers! built-in-reference-compilers))

;; for now defined in the DSL; later might become primitive and replace `host`.
(syntax-spec
  (nonterminal racket-expr #:description "racket expression"
    e:expr
    #:binding (host e)))

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
       (match variant-info
         [(or (simple-nonterm-info expander)
              (nesting-nonterm-info expander))
          #`(make-local-expand-entry-point
             #,expander)]
         [(? two-pass-nonterm-info?)
          (wrong-syntax #'ref "two-pass non-terminals may not be used as entry points")])]))
  
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
