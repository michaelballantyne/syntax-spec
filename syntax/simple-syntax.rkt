#lang racket/base

(provide simple-spec-expand)

(require "../syntax-spec/spec.rkt"
         "../syntax-spec/expand.rkt"
         "../binding-spec/spec.rkt"
         "../binding-spec/expand.rkt"
         
         (for-syntax racket/base
                     syntax/parse
                     syntax/parse/class/paren-shape))

;; (simple-spec-expand stx
;;   (_ ([x e]) b)
;;   [(! x mylang-binding)
;;    (: e mylang-expand-expr)
;;    (: b mylang-expand-expr)])
(define-syntax simple-spec-expand
  (syntax-parser
    [(_ e-stx
        sspec
        bspec)
     (with-syntax ([sspec-rt (compile-stx-spec #'sspec)]
                   [bspec-rt (compile-binding-spec #'bspec)])
       #'(simple-spec-expand-rt
          e-stx
          sspec-rt
          bspec-rt))]))

(define (simple-spec-expand-rt stx
                               stx-spec
                               binding-spec)
  (define vmap (deconstruct stx stx-spec))
  (define vmap^ (simple-expand binding-spec vmap))
  (reconstruct vmap stx stx-spec))

(begin-for-syntax
  (define (compile-stx-spec stx)
    (syntax-parse stx
      [(~datum _)
       #'(pany)]
      [pv:id
       #'(pvar 'pv)]
      [(a . d)
       #`(cons #,(compile-stx-spec #'a)
               #,(compile-stx-spec #'d))]
      [lit
       #''lit]))

  (define (compile-binding-spec stx)
    (syntax-parse stx
      #:datum-literals (~ : ! ^)
      [(~ svar pred)
       #'(ref 'svar pred "not bound as DSL var")]
      [(: svar nonterm)
       #'(subexp 'svar nonterm)]
      [(! svar bvalc)
       #'(bind 'svar bvalc)]
      [(~braces spec ...)
       (with-syntax ([(spec-c ...) (map compile-binding-spec
                                        (syntax->list #'(spec ...)))])
         #'(scope (group (list spec-c ...))))]
      [(~brackets spec ...)
       (with-syntax ([(spec-c ...) (map compile-binding-spec
                                        (syntax->list #'(spec ...)))])
         #'(group (list spec-c ...)))]))
  )
