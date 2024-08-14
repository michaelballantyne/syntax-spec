#lang typed/racket

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
 "private/compile.rkt")

(require
  "../../../main.rkt"
  (for-syntax
   "../../../private/syntax/syntax-classes.rkt"
   racket/base
   syntax/parse
   (rename-in syntax/parse [define/syntax-parse def/stx])))

(syntax-spec
  (binding-class var #:description "PEG variable")
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
    #:binding (nest-one ps e)

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
    #:binding (nest-one ps (scope (bind v) tail))

    (seq ps1:peg-seq ps2:peg-seq)
    #:binding (nest-one ps1 (nest-one ps2 tail))

    (alt e1:peg e2:peg)

    (? e:peg-seq)
    #:binding (nest-one e tail)

    (plain-alt e1:peg-seq e2:peg-seq)
    #:binding (nest-one e1 (nest-one e2 tail))

    (* ps:peg-seq)
    #:binding (nest-one ps tail)

    (src-span v:var ps:peg-seq)
    #:binding (scope (bind v) (nest-one ps tail))

    pe:peg-el)

  (nonterminal peg
    ps:peg-seq
    #:binding (nest-one ps []))

  (host-interface/definitions
   (define-pegs [name:nonterm p:peg] ...)
   #:binding (export name)
   #'(begin (define name (lambda ([in : text-rep]) (with-reference-compilers ([var immutable-reference-compiler])
                                        (compile-peg p in))))
            ...))

  (host-interface/expression
   (parse name:nonterm in-e:expr)
   #'(compile-parse name in-e)))

(define-syntax show-expanded
  (syntax-parser
    [(_ stx) #`'#,(local-expand #'stx 'module '())]))

; Interface macros

(define-syntax-rule (define-peg name peg) (define-pegs [name peg]))

; Default implementation of #%peg-datum interposition point

(define-syntax #%peg-datum
  (peg-macro
   (syntax-parser
     [(_ (~or* v:char v:string)) #'(text v)])))

(begin-for-syntax
  (define local-expand-peg (nonterminal-expander peg)))

(module+ test
  (require/typed rackunit
    [check-equal? (-> Any Any Any)]
    [check-exn (-> Any Any Any)])
  (define-peg foo "foo")
  (check-equal? (parse-result-value (parse foo "foo"))
                "foo")
  (define-peg paren-foo (=> (seq "(" (seq (bind x foo) ")"))
                            x))
  (check-equal? (parse-result-value (parse paren-foo "(foo)"))
                "foo")
  (define-peg foos (=> (* (bind x foo))
                       x))
  (check-equal? (parse-result-value (parse foos "foofoofoo"))
                '("foo" "foo" "foo"))
  (define-peg foobar (alt foo "bar"))
  (check-equal? (parse-result-value (parse foobar "bar"))
                '"bar")
  ; TODO figure out how to do type annotations nicely
  #;(define-type (Peg T) (-> text-rep (values text-rep T)))
  #;(define-type (Sexpr T) (U T (Listof (Sexpr T))))
  #;#;(: foo-sexpr (Peg (Sexpr String)))
  (define-peg foo-sexpr (alt foo (=> (seq "(" (seq (* (bind x foo-sexpr)) ")"))
                                     x))))
