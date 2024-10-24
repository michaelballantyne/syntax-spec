#lang racket/base

(provide
 current-orig-stx
 wrong-syntax/orig
 wrong-syntax/syntax-spec

 maybe-description
 maybe-binding-space
 maybe-reference-compiler

 nested-binding-syntax
 sspec-term
 bspec-term
 
 ;; Syntax class matching identifiers with a `:`
 ;;
 ;; Attributes:
 ;;   var - part before the `:`
 ;;   ref - part after the `:`
 ref-id

 ;; Syntax class matching identifiers that that do not have meaning
 ;; in the metalanguage and are thus usable as form names.
 form-id

 ;; Syntax class matching identifiers with no `:`
 nonref-id

 ;; Splicing syntax class matching production specs with optional
 ;; #:binding binding specs
 ;;
 ;; General form:
 ;;   (~seq sspec (~optional (~seq #:binding bspec)))
 ;;
 ;; Attributes:
 ;;   sspec
 ;;   bspec
 ;;   form-name - when sspec is an s-expression beginning with a nonref-id,
 ;;                 this attribute is bound to that id.
 production
 form-production
 syntax-production
 rewrite-production
 form-rewrite-production

 form-spec
 
 extclass-spec

 nonterminal-options

 compiler
 parse-body
 maybe-binding-decl)

(require
  racket/string
  racket/list
  racket/match
  syntax/parse
  syntax/srcloc
  racket/syntax)

(define-splicing-syntax-class (maybe-description name)
  (pattern (~optional (~seq #:description string-stx:string))
    #:attr str (or (attribute string-stx)
                   #`#,(symbol->string (syntax-e name)))))

(define-splicing-syntax-class maybe-binding-space
  (pattern (~optional (~seq #:binding-space stx:id) #:defaults ([stx #'#f]))
    #:attr sym (syntax-e (attribute stx))))

(define-splicing-syntax-class maybe-reference-compiler
  (pattern (~optional (~seq #:reference-compiler ~! compiler-stx))
           #:attr compiler (attribute compiler-stx)))

(define current-orig-stx (make-parameter #f))

;; Used for meta-level errors where there is no good more-specific form
;; to blame; that is, incorrect syntax-spec syntax, but on something like
;; spec-var:[nt] where the error is not related to the immediately surrounding
;; syntax. For errors where there is more appropriate immediately surrounding
;; syntax, we use plain `wrong-syntax`.
(define (wrong-syntax/syntax-spec
         stx #:extra [extras null] format-string . args)
  (parameterize ([current-syntax-context (datum->syntax #f 'syntax-spec)])
    (apply wrong-syntax stx #:extra extras format-string args)))

;; Used by DSL expanders to raise object-level errors; that is, a my-expression
;; was expected by my-interface-macro.
(define (wrong-syntax/orig stx #:extra [extras null] format-string . args)
  (parameterize ([current-syntax-context (current-orig-stx)])
    (apply wrong-syntax stx #:extra extras format-string args)))


(define-syntax-class sspec-term
  #:description "syntax spec term"
  (pattern _))

(define-syntax-class bspec-term
  #:description "binding spec term"
  (pattern _))

(define-syntax-class nested-binding-syntax
  #:description "pattern variable binding for nested syntax"
  (pattern (id:id)))

(define-syntax-class ref-id
  #:description "pattern variable with annotation"
  (pattern name:id #:when (has:? #'name)
    #:do [(define-values (var ref) (split: #'name))]
    #:attr var var
    #:attr ref ref))
  
(define-syntax-class nonref-id
  (pattern name:id #:when (not (has:? #'name))))

(define-syntax-class form-id
  (pattern name:nonref-id
    #:when (not (member (syntax-e #'name)
                        '(~datum ~literal ~> ~>/form ... ...+)))))

(define-splicing-syntax-class production
  #:description "nonterminal production"
  (pattern r:rewrite-production
    #:attr form-name #f)
  (pattern r:form-rewrite-production
    #:attr form-name (attribute r.form-name))
  (pattern p:form-production
    #:attr form-name (attribute p.form-name))
  (pattern p:syntax-production
    #:attr form-name (attribute p.form-name)))

(define-syntax-class form-rewrite-production
  #:description "form rewrite spec"
  #:datum-literals (~>/form)
  (pattern (~>/form (~and pat (form-name:id . _)) parse-body ... final-body)))

(define-syntax-class rewrite-production
  #:description "rewrite spec"
  #:datum-literals (~>)
  (pattern (~> pat parse-body ... final-body)))

(define-splicing-syntax-class form-production
  #:description "production spec"
  (pattern (~seq fspec:form-spec (~optional (~seq #:binding bspec)))
    #:attr form-name (attribute fspec.form-name)))

(define-splicing-syntax-class syntax-production
  #:description "production spec"
  (pattern (~seq sspec:syntax-spec (~optional (~seq #:binding bspec)))
    #:attr form-name #f))

(define-syntax-class form-spec
  #:description "form spec"
  (pattern (form-name:form-id . sspec:syntax-spec))
  (pattern form-name:form-id))

(define-syntax-class syntax-spec
  #:description "syntax spec"
  (pattern _))

; Is id a colon-separated pair of identifiers?
(define (has:? id)
  (define str (symbol->string (syntax-e id)))
  (match (string-split str ":" #:trim? #f)
    [(list before after)
     (and (non-empty-string? before) (non-empty-string? after))]
    [_ #f]))
  
(define (split: id)
  (define str (symbol->string (syntax-e id)))
  
  (define strs
    (string-split str ":" #:trim? #f))

  (define loc1
    (update-source-location
     id
     #:span (string-length (first strs))))

  (define loc2
    (let ([loc2-offset (+ (string-length (first strs)) 1)])
      (update-source-location
       id
       #:column (+ (syntax-column id) loc2-offset)
       #:position (and (syntax-position id)
                       (+ (syntax-position id)
                          loc2-offset))
       #:span (string-length (second strs)))))
  
  (values (datum->syntax id (string->symbol (first strs)) loc1 id)
          (datum->syntax id (string->symbol (second strs)) loc2 id)))


(define-splicing-syntax-class extclass-spec
  (pattern v:id #:attr [classes 1] (list #'v))
  (pattern (classes:id ...)))

(define-splicing-syntax-class nonterminal-options
  (pattern (~seq (~optional (~seq #:description description:string))
                 (~optional (~seq #:bind-literal-set litset-binder:id))
                 (~optional (~seq #:allow-extension extensions:extclass-spec))
                 (~var maybe-space maybe-binding-space))
    #:attr space-stx (attribute maybe-space.stx)
    #:attr space-sym (attribute maybe-space.sym)
    #:attr ext-classes (if (attribute extensions) (attribute extensions.classes) '())))

(define-splicing-syntax-class compiler
  #:description "host interface compiler"
  (pattern (~seq body:parse-body ...+)))

(define-syntax-class parse-body
  #:description "pattern directive or body"
  (pattern _))
  
(define-splicing-syntax-class maybe-binding-decl
  (pattern (~optional (~seq #:binding ~! bspec))))
