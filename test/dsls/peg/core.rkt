#lang racket/base

(provide
 ; core forms
 eps
 seq
 alt
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
 parse

 ; default #%peg-datum implementation
 #%peg-datum

 make-text
 ; result datatype
 (struct-out parse-result)

 ; interfaces for macro definitions and local-expansion
 (for-syntax
  ;local-expand-peg
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
   "private/leftrec-check.rkt"
   "private/compile.rkt"))

(require
  syntax-spec
  (for-syntax
   racket/base
   syntax/parse
   racket/syntax
   syntax/id-table
   (rename-in syntax/parse [define/syntax-parse def/stx])))

(syntax-spec
  (binding-class var #:description "PEG variable")
  (binding-class nonterm #:description "PEG nonterminal")
  (extension-class peg-macro #:description "PEG macro")

  (nonterminal peg-el
    #:description "PEG expression"
    #:allow-extension peg-macro

    (~literal eps)
    n:nonterm
    ((~literal char) e:expr)
    ((~literal token) e:expr)
    ((~literal !) e:peg)

    (~> s:string #'(text s))
    ((~literal text) e:racket-expr)

    ((~literal =>) ps:peg-seq e:racket-expr)
    #:binding (nest-one ps e))

  (nonterminal/nesting peg-seq (tail)
    #:description "PEG expression"
    #:allow-extension peg-macro

    ((~literal bind) v:var ps:peg-seq)
    ; TODO doesn't this mean the var is in scope for ps? that doesn't make sense
    #:binding {(bind v) (nest-one ps tail)}
    ; I feel like it should be
    #;[{(bind v) tail} (nest-one ps tail)]
    ; but then you have non-linear use of tail

    ((~literal seq) ps1:peg-seq ps2:peg-seq)
    #:binding (nest-one ps1 (nest-one ps2 tail))

    ; this originally didn't bind anything, but tests expect it to bind everything
    ; failed alts bind vars to #f
    ((~literal alt) e1:peg-seq e2:peg-seq)
    #:binding (nest-one e1 (nest-one e2 tail))

    ((~literal *) ps:peg-seq)
    #:binding (nest-one ps tail)

    ((~literal src-span) v:var ps:peg-seq)
    #:binding {(bind v) (nest-one ps tail)}

    pe:peg-el)

  (nonterminal peg
    ps:peg-seq
    #:binding (nest-one ps []))

  (host-interface/definitions
   (define-pegs [name:nonterm p:peg] ...)
   #:binding (export name)
   ; TODO leftrec check
   #'(begin (define name (lambda (in) (with-reference-compilers ([var immutable-reference-compiler])
                                        (compile-peg/macro p in))))
            ...))

  (host-interface/expression
   (parse name:nonterm in-e:expr)
   (compile-parse #'name #'in-e)))

; needed to make this a macro for ellipsis depth stuff
(define-syntax compile-peg/macro
  (syntax-parser
    [(_ p in) (compile-peg #'p #'in)]))

; Interface macros

(define-syntax-rule (define-peg name peg) (define-pegs [name peg]))
#;
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

#;
(define-syntax define-peg-pass2
  (syntax-parser
    [(_ name peg-e)
     (define-values (peg-e^) (expand-peg #'peg-e))
     (lift-leftrec-check! #'name peg-e^)
     #'(begin)]))

#;
(define-syntax define-peg-rhs
  (syntax-parser
    [(_ name)
     (define e (free-id-table-ref expanded-defs (syntax-local-introduce #'name)))
     (define e^ (compile-peg e #'in))
     #`(lambda (in) #,e^)]))

#;
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
