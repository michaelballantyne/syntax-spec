#lang scribble/manual

@(require scribble/example
          (for-label racket "../../main.rkt" syntax/id-table syntax/transformer))

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

@defproc[(make-variable-like-reference-compiler [reference-stx (or/c syntax? (-> identifier? syntax?))]
                                                [setter-stx (or/c syntax? (-> syntax? syntax?)) #f])
        set!-transformer?]

Like @racket[make-variable-like-transformer].

If @racket[reference-stx] is syntax, replace references with it.
If @racket[reference-stx] is a procedure, apply it to the reference syntax.

If @racket[setter-stx] is syntax, it should be syntax for a procedure which receives
the new value for the variable.
If @racket[setter-stx] is a procedure, apply it to the entire @racket[set!] expression.

When the identifier is used in an application position,
wrap the reference with @racket[#%expression].

Here is an example for a @racket[match] DSL where pattern-bound variables cannot be mutated:

@;TODO host-interface/expression and racket-expr isn't getting linked
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

Defines a (mutable) symbol table for global use. For example,
if your DSL has a static type checker and you're requiring typed identifiers between modules,
you can store each identifier's type in a persistent symbol table.

@;TODO mention how it's saved in the compiled code?

Can only be used at the top-level of a module.

@defform[(define-local-symbol-table id)]

Defines (mutable) a symbol table for local use.

@defproc[(syntax-datum? [v any/c]) boolean?]

@defproc[(symbol-table-set! [table any/c]
                            [id identifier?]
                            [v (or/c syntax? syntax-datum?)]) void?]

Like @racket[free-id-table-set!]

@defproc[(symbol-table-ref [table any/c] [id identifier?] [failure any/c]) any/c]

Like @racket[free-id-table-ref]

@section{Binding Operations}

@defproc[(compiled-identifier=? [a-id identifier?] [b-id identifier?]) boolean?]

@;TODO run this by michael, not sure how to explain it.

Returns @racket[#t] if the two compiled DSL identifiers correspond to the same binding, returns @racket[#f] otherwise. Similar to @racket[free-identifier=?].

This is the equality used by symbol tables.

@defproc[(free-identifiers [stx syntax?] [#:allow-host? allow-host? boolean? #f]) (listof identifier?)]

Get a DSL expression's free identifiers (deduplicated).

Host expressions currently are not supported.

@defproc[(alpha-equivalent? [stx-a syntax?] [stx-b syntax?] [#:allow-host? allow-host? boolean? #f]) boolean?]

Returns @racket[#t] if the two DSL expressions are alpha-equivalent, @racket[#f] otherwise.

Host expressions currently are not supported.

@defform[(get-racket-referenced-identifiers [binding-class-id ...] expr)]

Returns an immutable symbol set containing identifiers of the specified binding classes that were referenced in racket (host) expressions in @racket[expr].

@section{Expansion}

@defform[(nonterminal-expander nonterminal-id)
         #:contracts ([nonterminal-id identifier?])]

Produces an expander procedure for the specified nonterminal. This procedure expands macros down to the DSL's core forms,
binds identifiers in binding positions, and can be configured to compile and rename identifiers. It does not expand host expressions.

Expander procedure has contract @racket[(->* (syntax?) (#:should-rename? boolean?) syntax?)].
The default behavior is not to re-compile and re-rename identifiers. To do this, pass in @racket[#:should-rename? #t].

Can only be used with simple non-terminals.

@;TODO have a collection of running examples, and just require the pattern matching dsl here?

@examples[
(module arithmetic racket
  (require syntax-spec)
  (syntax-spec
    (extension-class arithmetic-macro)
    (nonterminal arithmetic
      #:allow-extension arithmetic-macro
      ((~literal +) a:arithmetic b:arithmetic)
      ((~literal *) a:arithmetic b:arithmetic)
      n:number))
  (define-syntax sqr
    (arithmetic-macro
     (syntax-rules ()
       [(sqr n) (* n n)])))
  (begin-for-syntax
    (define local-expand-arithmetic (nonterminal-expander arithmetic))
    (displayln (local-expand-arithmetic #'(sqr 1)))))
]
