#lang racket/base

(provide
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
 )


(require
  racket/string
  racket/list
  syntax/parse
  racket/syntax)


(define-syntax-class ref-id
  #:description "pattern variable with annotation"
  (pattern name:id #:when (has:? #'name)
           #:do [(define-values (var ref) (split: #'name))]
           #:attr var var
           #:attr ref ref))
  
(define-syntax-class nonref-id
  (pattern name:id #:when (not (has:? #'name))))


(define-splicing-syntax-class production-spec
  (pattern (~seq sspec:sspec (~optional (~seq #:binding bspec)))
           #:attr form-name (attribute sspec.form-name)))

(define-syntax-class sspec
  (pattern (form-name:nonref-id . _))
  (pattern _ #:attr form-name #f))

  
(define (has:? id)
  (string-contains?
   (symbol->string (syntax-e id))
   ":"))
  
(define (split: id)
  (define strs
    (string-split (symbol->string (syntax-e id)) ":" #:trim? #f))
  (values (datum->syntax id (string->symbol (first strs)) id id)
          (datum->syntax id (string->symbol (second strs)) id id)))

(define-splicing-syntax-class extclass-spec
  (pattern v:id #:attr [classes 1] (list #'v))
  (pattern (classes:id ...)))
