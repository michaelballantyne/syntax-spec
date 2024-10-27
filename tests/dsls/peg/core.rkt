#lang racket/base

(provide
 ; core forms
 eps
 seq
 alt
 plain-alt
 ?
 *
 !
 =>
 token
 text
 char
 (rename-out [bind :]
             [src-span :src-span])

 ; interface macros
 define-peg
 define-pegs
 parse

 ; default #%peg-datum implementation
 #%peg-datum

 make-text
 ; result datatype
 (struct-out parse-result)

 ; interfaces for macro definitions and local-expansion
 (for-syntax
  local-expand-peg
  peg-macro))

(require
  "private/forms.rkt"
  "private/runtime.rkt"
  "private/compile.rkt"
  (for-syntax
   "private/leftrec-check.rkt"))

(require
  "../../../main.rkt"
  (for-syntax
   "../../../private/syntax/syntax-classes.rkt"
   racket/base
   syntax/parse
   (rename-in syntax/parse [define/syntax-parse def/stx])))

(syntax-spec
  (binding-class var #:description "PEG variable" #:reference-compiler immutable-reference-compiler)
  (binding-class nonterm #:description "PEG nonterminal")
  (extension-class peg-macro #:description "PEG macro")

  (nonterminal peg-el
    #:description "PEG expression"
    #:allow-extension peg-macro

    eps
    (char e:expr)
    (token e:expr)
    (! e:peg)

    (~> (~or s:string s:char s:number s:regexp)
        #:with #%peg-datum (datum->syntax #'s '#%peg-datum)
        #'(#%peg-datum s))

    (text e:text-expr)

    (=> ps:peg-seq e:racket-expr)
    #:binding (nest ps e)

    (~> n:id #'(#%nonterm-ref n))
    (#%nonterm-ref n:nonterm))

  (nonterminal text-expr
    s:string
    e:racket-expr)

  (nonterminal/nesting peg-seq (tail)
    #:description "PEG expression"
    #:allow-extension peg-macro

    (~> p:ref-id #'(bind p.var p.ref))

    (bind v:var ps:peg-seq)
    #:binding (nest ps (scope (bind v) tail))

    (seq ps1:peg-seq ps2:peg-seq)
    #:binding (nest ps1 (nest ps2 tail))

    (alt e1:peg e2:peg)

    (? e:peg-seq)
    #:binding (nest e tail)

    (plain-alt e1:peg-seq e2:peg-seq)
    #:binding (nest e1 (nest e2 tail))

    (* ps:peg-seq)
    #:binding (nest ps tail)

    (src-span v:var ps:peg-seq)
    #:binding (scope (bind v) (nest ps tail))

    pe:peg-el)

  (nonterminal peg
    ps:peg-seq
    #:binding (nest ps []))

  (host-interface/definitions
   (define-pegs [name:nonterm p:peg] ...)
   #:binding [(export name) ...]
   (run-leftrec-check! (attribute name) (attribute p))
   #'(begin (define name (lambda (in) (compile-peg p in)))
            ...))

  (host-interface/expression
   (parse name:nonterm in-e:expr)
   #'(compile-parse name in-e)))

; Interface macros

(define-syntax-rule (define-peg name peg) (define-pegs [name peg]))

; Default implementation of #%peg-datum interposition point

(define-syntax #%peg-datum
  (peg-macro
   (syntax-parser
     [(_ (~or* v:char v:string)) #'(text v)])))

(begin-for-syntax
  (define local-expand-peg (nonterminal-expander peg)))
