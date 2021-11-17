#lang racket/base

(provide
 current-orig-stx
 wrong-syntax/orig

 nested-binding-syntax
 sspec-term
 bspec-term
 
 ;; Syntax class matching identifiers with a `:`
 ;;
 ;; Attributes:
 ;;   var - part before the `:`
 ;;   ref - part after the `:`
 ref-id

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
 production-spec

 extclass-spec

 nonterminal-options
 )


(require
  racket/string
  racket/list
  syntax/parse
  syntax/srcloc
  racket/syntax)

(define current-orig-stx (make-parameter #f))

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


(define-splicing-syntax-class production-spec
  #:description "production spec"
  (pattern (~seq sspec:sspec (~optional (~seq #:binding bspec)))
           #:attr form-name (attribute sspec.form-name)))

(define-syntax-class sspec
  #:description "syntax spec"
  (pattern (form-name:nonref-id . _))
  (pattern _:expr #:attr form-name #f))

  
(define (has:? id)
  (string-contains?
   (symbol->string (syntax-e id))
   ":"))
  
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
                 (~optional (~seq #:allow-extension extensions:extclass-spec)))
           #:attr ext-classes (if (attribute extensions) (attribute extensions.classes) '())))