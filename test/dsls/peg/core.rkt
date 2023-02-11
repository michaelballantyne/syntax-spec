#lang racket/base

(provide
 ; core forms
 eps
 seq
 alt
 *
 !
 :
 =>
 token
 text
 char
 :src-span

 ; interface macros
 define-peg
 parse

 ; default #%peg-datum implementation
 #%peg-datum

 make-text
 ; result datatype
 (struct-out parse-result)

 ; interfaces for macro definitions and local-expansion
 (for-syntax
  local-expand-peg
  peg-macro
  gen:peg-macro
  peg-macro?
  peg-macro/c
  peg-macro-transform
  peg
  peg-literals))

(require
  "private/forms.rkt"
  "private/runtime.rkt"
  (for-syntax
   "private/env-reps.rkt"
   "private/syntax-classes.rkt"
   "private/expand.rkt"
   "private/leftrec-check.rkt"
   "private/compile.rkt"))

(require
  (for-syntax
   racket/base
   syntax/parse
   racket/syntax
   ee-lib
   syntax/id-table
   (rename-in syntax/parse [define/syntax-parse def/stx])))

; Interface macros

(define-syntax define-peg
  (module-macro
    (syntax-parser
      [(_ name:id peg-e:peg)
       (when (not (eq? 'module (syntax-local-context)))
         (raise-syntax-error #f "define-peg only works in module context" this-syntax))
       (def/stx impl (generate-temporary #'name))
       (syntax-local-lift-module-end-declaration
        #'(define-peg-pass2 name peg-e))
       #'(begin
           (define impl (define-peg-rhs name))
           (begin-for-syntax
             (record-compiled-id! #'name #'impl))
           (define-syntax name (peg-non-terminal-rep)))])))

(define-syntax define-peg-pass2
  (syntax-parser
    [(_ name peg-e)
     (define-values (peg-e^) (expand-peg #'peg-e))
     (lift-leftrec-check! #'name peg-e^)
     #'(begin)]))

(define-syntax define-peg-rhs
  (syntax-parser
    [(_ name)
     (define e (free-id-table-ref expanded-defs (syntax-local-introduce #'name)))
     (define e^ (compile-peg e #'in))
     #`(lambda (in) #,e^)]))

(define-syntax parse
  (expression-macro
    (syntax-parser
      [(_ peg-name:nonterm-id in-e:expr)
       (compile-parse #'peg-name #'in-e)])))

; Default implementation of #%peg-datum interposition point

(define-syntax #%peg-datum
  (peg-macro
   (syntax-parser
     [(_ (~or* v:char v:string)) #'(text v)])))
