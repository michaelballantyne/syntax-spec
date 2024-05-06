#lang scribble/manual

@(require (for-label racket "../../main.rkt"))

@;-----------------------

@(define (tech/reference str)
   (tech #:doc '(lib "scribblings/reference/reference.scrbl") str))

@(define (seclink/reference sec str)
   (seclink sec #:doc '(lib "scribblings/reference/reference.scrbl") str))

@;-----------------------

@title{Compiling languages}

@section{Compiling references to DSL bindings within Racket code}

@defform[(with-reference-compilers
             ([binding-class-id reference-compiler-expr] ...)
           body ...+)]

Declares which reference compiler to use when expanding DSL-bound identifiers of the specified binding classes when expanding
@racket[body]. Evaluates to @racket[body].

@defthing[immutable-reference-compiler set!-transformer?]

Raises a syntax error when identifiers are used in @racket[set!] expressions.

@defthing[mutable-reference-compiler set!-transformer?]

Allows identifiers to be used in @racket[set!] expressions. Identifiers behave as they usually do in plain Racket.

Here is an example for a @racket[match] DSL where pattern-bound variables cannot be mutated:

@racketblock[
(syntax-spec
 (host-interface/expression
  (match target:racket-expr c:clause ...)
  #'(with-reference-compilers ([pat-var immutable-reference-compiler])
      (let ([target-pv target])
        (match-clauses target-pv c ...)))))
]


@section{Symbol tables}

@defform[(define-persistent-symbol-table id)]

Can only be used in module context.

@defform[(define-local-symbol-table id)]

@defproc[(syntax-datum? [v any/c]) boolean?]

@defproc[(symbol-table-set! [table any/c]
                            [id identifier?]
                            [v (or/c syntax? syntax-datum?)]) void?]

@defproc[(symbol-table-ref [table any/c] [id identifier?] [failure any/c]) any/c]

