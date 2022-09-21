#lang racket/base

#|
This is a regression test.
The bug was that host exprs couldn't use set! on dsl-bound variables.

Previously, if a dsl's compiler bound variables using a regular 'let', using set! resulted in this error:
compile-reference: contract violation
  expected: identifier?
  given: #<syntax:compile-to-set!-binder.rkt:47:43 (set! x 2)>

Previously, if a dsl's compiler bound variables as set!-transformers, references worked fine,
but using the variable in set! form did not work.
It would result in an error like "set!: cannot mutate syntax identifier".
It's the same error as trying to set! an identifier defined by define-syntax.

In the old compiler, variables would be bound to simple transformers in the environment, rather than
set!-transformers.
|#

(require "../../testing.rkt")

(define-hosted-syntaxes
  (binding-class var #:description "mylet variable")

  (nonterminal mylet-expr
               #:description "mylet expression"
               (mylet v:var body:expr)
               #:binding {(bind v) (host body)}
               (regular-let v:var val:expr body:expr)
               #:binding [(host val) {(bind v) (host body)}]
               (mybegin def:mydef body:expr)
               #:binding {(recursive def) (host body)})
  (two-pass-nonterminal mydef
               #:description "mylet definition"
               (mydefine-inner v:var)
               #:binding (export v)
               (regular-define-inner v:var val:expr)
               #:binding [(export v) (host val)]))

(define-host-interface/expression (run le:mylet-expr)
  (compile-mylet-expr #'le))

(define-for-syntax compile-mylet-expr
  (syntax-parser
    [(mylet v:id body:expr)
     #:with var^ (compile-binder! #'v)
     #:with body^ (resume-host-expansion #'body #:reference-compilers ([var compile-reference]))
     #'(let-syntax ([var^ (make-set!-transformer
                           (syntax-parser
                             [(set! x:id val:expr) #'(list 'set! val)]
                             [x:id #''ref]))])
         body^)]
    [(regular-let var:id val:expr body:expr)
     #:with var^ (compile-binder! #'var)
     #:with val^ (resume-host-expansion #'val #:reference-compilers ([var compile-reference]))
     #:with body^ (resume-host-expansion #'body #:reference-compilers ([var compile-reference]))
     #'(let ([var^ val^]) body^)]
    [(mybegin ((~literal mydefine-inner) var) body) (compile-mylet-expr #'(mylet var body))]
    [(mybegin ((~literal regular-define-inner) var val) body) (compile-mylet-expr #'(regular-let var val body))]))

; dsl-defined macros are not supported
#;(define-host-interface/definition (mydefine v:var)
  #:binding [(export v)]
  ->
  ; this has to be 'define'
  (define-syntax
    [(compile-binder! #'v)]
    [(make-set!-transformer
      (syntax-parser
        [(set! x:id val:expr) #'(list 'set! val)]
        [x:id #''ref]))]))

(define-host-interface/definition (regular-define v:var val:expr)
  #:binding [(export v) (host val)]
  ->
  (define
    [(compile-binder! #'v)]
    [(resume-host-expansion #'val #:reference-compilers ([var compile-reference]))]))

(define-host-interface/expression (ref v:var)
  (compile-reference #'v))

; allows set! of dsl vars in regular racket expressions
(define-host-interface/expression (my-set! v:var val:expr)
  #:binding (host val)
  #:with var^ (compile-reference #'v)
  #:with val^ (resume-host-expansion #'val #:reference-compilers ([var compile-reference]))
  #'(set! var^ val^))

; this used to pass
(check-equal? (run (mylet x x)) 'ref)
; this used to pass
(check-equal? (run (regular-let x 1 x)) 1)
; this used to pass
(check-equal? (run (mybegin (mydefine-inner x) x)) 'ref)
; this used to pass
(check-equal? (run (mybegin (regular-define-inner x 1) x)) 1)
#;(let ()
    (mydefine x)
    (check-equal? x 'ref))
; this used to pass
(let ()
  (regular-define x 1)
  (check-equal? (ref x) 1))
; this used to pass, but it doesn't really test set! behavior because it uses my-set!
(let ()
  (regular-define x 1)
  (my-set! x 2)
  (check-equal? (ref x) 2))
; this used to fail
(check-equal? (run (mylet x (set! x 2))) (list 'set! 2))
; this used to fail
(check-equal? (run (regular-let x 1 (begin (set! x 2) x))) 2)
; this used to fail with the 'expected identifier' error
(check-equal? (run (mybegin (mydefine-inner x) (set! x 2))) (list 'set! 2))
; this used to fail with the 'expected identifier' error
(check-equal? (run (mybegin (regular-define-inner x 1) (begin (set! x 2) x))) 2)
(test-equal? "using dsl-bound variable as a function"
             (run (regular-let x add1 (x 2)))
             3)
