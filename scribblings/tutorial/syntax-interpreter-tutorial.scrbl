#lang scribble/manual

@(require (for-label racket racket/block racket/class racket/match racket/list syntax/parse "../../main.rkt")
          scribble/example
          racket/sandbox)
@(define eval (make-base-eval '(require racket)))
@(define-syntax-rule (repl body ...) (examples #:eval eval #:label #f body ...))

@title[#:tag "syntax-interpreter"]{Advanced Tutorial: An Interpreted Language}

This guide demonstrates how to use syntax-spec to create an interpreted language, as well as the benefits of this approach.

Typically, syntax-spec is used to create languages that compile to Racket. However, it's possible to use it to create an interpreted language as well. In such an implementation, syntax-spec will enforce the grammar, check binding, macro-expand the source program, etc. and pass off the expanded, core syntax to an interpreter that evaluates it to a value. As an example, let's create an interpreted implementation of the lambda calculus.

@section{Expander}

Here is the syntax-spec:

@repl[
#:hidden #t
(module grammar racket
  (provide (all-defined-out) (for-syntax (all-defined-out)) (for-space lc (all-defined-out)))
  (require "main.rkt" (for-syntax syntax/parse))
  (syntax-spec
    (binding-class lc-var #:binding-space lc)
    (extension-class lc-macro #:binding-space lc)
    (nonterminal lc-expr
      #:binding-space lc
      #:allow-extension lc-macro
      n:number
      (+ e1:lc-expr e2:lc-expr)
      x:lc-var
      (lambda (x:lc-var) e:lc-expr)
      #:binding (scope (bind x) e)
      (rkt e:expr)
      (~> (e1 e2)
          (syntax/loc this-syntax (#%app e1 e2)))
      (#%app e1:lc-expr e2:lc-expr))

    (host-interface/expression
      (lc-expand e:lc-expr)
      #'#'e)))
(require 'grammar "main.rkt" (for-syntax syntax/parse))
]

@racketmod[
racket
(require syntax-spec-dev (for-syntax syntax/parse))
(syntax-spec
  (binding-class lc-var #:binding-space lc)
  (extension-class lc-macro #:binding-space lc)
  (nonterminal lc-expr
    #:binding-space lc
    #:allow-extension lc-macro
    n:number
    (+ e1:lc-expr e2:lc-expr)
    x:lc-var
    (lambda (x:lc-var) e:lc-expr)
    #:binding (scope (bind x) e)
    (~> (e1 e2)
        (syntax/loc this-syntax (#%app e1 e2)))
    (#%app e1:lc-expr e2:lc-expr))

  (host-interface/expression
    (lc-expand e:lc-expr)
    #'#'e))
]

We have numbers, binary addition, variables, lambdas, and applications. The interesting bit is the host interface @racket[lc-expand]. The result is the expanded syntax itself! For example:

@repl[
(lc-expand ((lambda (x) x) 1))
]

The host-interface could also just invoke the interpreter directly on the syntax:

@racketblock[
(host-interface/expression
    (lc e:lc-expr)
    #'(lc-eval #'e empty-env))
]

This is what you'd normally do, but we haven't implemented @racket[lc-eval] yet so we'll stick with @racket[lc-expand].

Our language is also macro-extensible, so the result of @racket[lc-expand] expands macros away:

@repl[
(eval:no-prompt (require (for-syntax syntax/parse)))
(eval:no-prompt
(define-dsl-syntax let lc-macro
  (syntax-parser
    [(_ ([x:id e:expr]) body:expr)
     #'((lambda (x) body) e)])))
(lc-expand (let ([x 1]) (+ x x)))
]

@section{Interpreter}

We can use the output of @racket[lc-expand] as the input of an interpreter. But first, let's define some helpers.

We will be building a strict, environment-based interpreter for this language, so we need to define what our environment will look like.

The environment needs to map variables to values, and we have the result of syntax-spec expansion, so we can use symbol tables! This means our environment will map identifiers to values and will respect hygiene. syntax-spec gives us this benefit of hygienic environments for free, which is important. If we used a hash from symbols to values, which would contain no binding/hygiene information, an example like this would break:

@racketblock[
(define-dsl-syntax m lc-macro
  (syntax-parser
    [(_ e) #'(let ([tmp 2]) e)]))
(lc (let ([tmp 1]) (m tmp)))
]

The macro-introduced @racket[tmp] would shadow the surface syntax @racket[tmp] and we'd get @racket[2] instead of @racket[1].

@repl[
#:hidden #t
(require (for-template "main.rkt"))
]

@repl[
(eval:alts (eval:no-prompt (require (for-template syntax-spec))) (void))
;; An Env is a (ImmutableBoundIdTable Value)
(eval:no-prompt (define empty-env (immutable-symbol-table)))
;; Env Identifier -> Value
(eval:no-prompt
(define (env-lookup env x)
  (if (symbol-table-has-key? env x)
      (symbol-table-ref env x)
      x)))
;; Env Identifier Value -> Void
(eval:no-prompt
(define (env-extend env x v)
  (symbol-table-set env x v)))
]

One more thing we'll need is the ability to raise errors. Luckily, since we're operating on syntax, we can report the source location of an error.

@repl[
(eval:no-prompt (require racket/syntax-srcloc))
(eval:no-prompt
(define (lc-error stx msg)
  (define loc (syntax-srcloc stx))
  (if loc
      (raise-user-error (format "~a: ~a" (srcloc->string loc) msg))
      (raise-user-error 'lc msg))))
(eval:error (lc-error #'x "something went wrong"))
]

Alright, now let's define our interpreter:

@repl[
(eval:no-prompt (require syntax/parse))
(eval:no-prompt
(define-syntax-rule (lc e)
  (lc-eval (lc-expand e) empty-env)))
(eval:no-prompt
(define (lc-eval stx env)
  (syntax-parse stx
    #:datum-literals (+ lambda #%app)
    [n:number
     (syntax->datum #'n)]
    [(+ e1 e2)
     (define v1 (lc-eval #'e1 env))
     (unless (number? v1)
       (lc-error this-syntax "+ expects number"))
     (define v2 (lc-eval #'e2 env))
     (unless (number? v2)
       (lc-error this-syntax "+ expects number"))
     (+ v1 v2)]
    [x:id
     (env-lookup env #'x)]
    [(lambda (x:id) e:expr)
     (lambda (v) (lc-eval #'e (env-extend env #'x v)))]
    [(#%app e1 e2)
     (match (lc-eval #'e1 env)
       [(? procedure? f)
        (f (lc-eval #'e2 env))]
       [_
        (lc-error this-syntax "applied non-function")])])))
(lc 1)
(lc (+ 1 1))
(lc (let ([x 1]) (+ x x)))
(lc ((lambda (x) (+ x 1)) 3))
(eval:error (lc (1 2)))
(define-dsl-syntax m lc-macro
  (syntax-parser
    [(_ e) #'(let ([tmp 2]) e)]))
(lc (let ([tmp 1]) (m tmp)))
]

Pretty cool!

To recap, we are using syntax-spec as a frontend for our interpreter, which operates on expanded syntax and uses symbol tables as an environment.

Here are some of the benefits of writing an interpreter in this style:

@itemlist[
@item{We operate on syntax, which means we easily get source locations in errors and we can use syntax/parse to perform case analysis on expressions}
@item{Our interpreter can assume that the program is grammatically valid and well-bound since we are operating on the result of expansion from syntax-spec}
@item{We can use symbol tables for environments, which are hygienic}
@item{Our language is macro-extensible and our interpreter only has to operate on core forms}
]

@section{Supporting Racket Subexpressions}

We can add limited support for Racket subexpressions to our language:

@racketblock[
(syntax-spec
  (nonterminal lc-expr
    #:binding-space lc
    #:allow-extension lc-macro
    n:number
    (+ e1:lc-expr e2:lc-expr)
    x:lc-var
    (lambda (x:lc-var) e:lc-expr)
    #:binding (scope (bind x) e)
    (rkt e:expr)
    (~> (e1 e2)
        ;; this is necessary to preserve source location, properties, etc.
        (syntax/loc this-syntax (#%app e1 e2)))
    (#%app e1:lc-expr e2:lc-expr)))
]

We added @racket[(rkt e:expr)] to the productions. Usually for racket expressions, we use @racket[racket-expr], which wraps the expression with @racket[#%host-expression]. This does some work behind the scenes to make sure we can refer to DSL bindings in the Racket expression. But for this syntax interpreter, that won't work, so we'll just use @racket[expr] to avoid wrapping the expression in a @racket[#%host-expression]. Evaluation is simple:

@repl[
(eval:no-prompt
(define (lc-eval stx env)
  (syntax-parse stx
    #:datum-literals (+ lambda #%app rkt)
    [n:number
     (syntax->datum #'n)]
    [(+ e1 e2)
     (define v1 (lc-eval #'e1 env))
     (unless (number? v1)
       (lc-error this-syntax "+ expects number"))
     (define v2 (lc-eval #'e2 env))
     (unless (number? v2)
       (lc-error this-syntax "+ expects number"))
     (+ v1 v2)]
    [x:id
     (env-lookup env #'x)]
    [(lambda (x:id) e:expr)
     (lambda (v) (lc-eval #'e (env-extend env #'x v)))]
    [(#%app e1 e2)
     (match (lc-eval #'e1 env)
       [(? procedure? f)
        (f (lc-eval #'e2 env))]
       [_
        (lc-error this-syntax "applied non-function")])]
    [(rkt e)
     (eval #'e)])))
(lc (rkt (* 4 2)))
]

We just add a case that calls @racket[eval] on the Racket expression! However, there are some limitations with this method. In particular, we have access to top-level names like @racket[*], but not local variables defined outside of the Racket subexpression, because @racket[eval] is evaluating against the global namespace and not capturing local variable definitions.

@repl[
(define top-level-x 2)
(lc (rkt top-level-x))
(eval:error
  (let ([local-x 3])
    (lc (rkt local-x))))
]

Similarly, we cannot reference @racket[lc-var]s:

@repl[
(eval:error
  (lc (let ([lc-x 4]) (rkt lc-x))))
]
