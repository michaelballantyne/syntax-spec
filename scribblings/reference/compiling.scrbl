#lang scribble/manual

@(require scribble/example
          (for-label racket "../../main.rkt" syntax/id-table syntax/id-set syntax/transformer))

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

@section{Compiled identifiers vs surface syntax}

The syntax of a DSL program in its initial, un-expanded, un-compiled state is called the @deftech{surface syntax}. During the expansion of a host interface usage, before your compiler is invoked, syntax-spec renames and compiles surface identifiers. The resulting identifiers are called @deftech{compiled identifiers} and have unique names and have special scopes according to your DSL's binding rules.

Another concept that comes up when discussing identifiers and compilation is positive vs negative space. This has to do with @tech/reference{macro-introduction scopes}. To ensure macro hygiene, the Racket expander distinguishes between syntax that was introduced by a macro and syntax that originated from elsewhere. To do this, it adds an introduction scope to the macro invocation's syntax, expands the invocation by running the macro's transformer, and then flips the scope, removing it from syntax that has it and adding it to syntax that doesn't.

During the expansion of the invocation, when a macro's transformer is running, the macro transformer will see this introduction scope on the incoming syntax. This syntax is in @deftech{negative space}. After the scope is flipped off on the result, the syntax is in @deftech{positive space}. It's important to note that positive vs negative space depends on the @emph{current} introduction scope, as there may be many introduction scopes floating around. It is also possible to manually flip this scope in a transformer to convert syntax between positive and negative space.

@section{Symbol collections}

Symbol collections allow compilers to track information related to dsl variables. Symbol collections expect to receive @tech{compiled identifiers} in @tech{negative space}.

@subsection{Symbol tables}

@defproc[(symbol-table? [v any/c]) boolean?]

Returns @racket[#t] if @racket[v] is a symbol table, @racket[#f] otherwise.

@defproc[(in-symbol-table [table symbol-table?]) (sequence/c identifier? (or/c syntax-datum? syntax?))]

Like @racket[in-free-id-table].

@defproc[(mutable-symbol-table? [v any/c]) boolean?]

Returns @racket[#t] if @racket[v] is a mutable symbol table, @racket[#f] otherwise.

@defform[(define-persistent-symbol-table id)]

Defines a (mutable) symbol table for global use. For example,
if your DSL has a static type checker and you're requiring typed identifiers between modules,
you can store each identifier's type in a persistent symbol table.

@;TODO mention how it's saved in the compiled code?

Can only be used at the top-level of a module.

@defproc[(local-symbol-table) mutable-symbol-table?]

Creates a (mutable) symbol table for local use.

@defproc[(syntax-datum? [v any/c]) boolean?]

Roughly, returns @racket[#t] if @racket[v] is something that could be the result of @racket[syntax->datum], @racket[#f] otherwise.

This includes pairs, vectors, symbols, numbers, booleans, etc.

@defproc[(symbol-table-set! [table mutable-symbol-table?]
                            [id identifier?]
                            [v (or/c syntax? syntax-datum?)]) void?]

Like @racket[free-id-table-set!].

@defproc[(symbol-table-ref [table symbol-table?] [id identifier?] [failure any/c]) any/c]

Like @racket[free-id-table-ref]

@defproc[(symbol-table-has-key? [table symbol-table?] [id identifier?]) boolean?]

Returns @racket[#t] if @racket[table] has an entry for @racket[id], @racket[#f] otherwise.

@defproc[(immutable-symbol-table? [v any/c]) boolean?]

Returns @racket[#t] if @racket[v] is an immutable symbol table, @racket[#f] otherwise.

@defproc[(immutable-symbol-table) immutable-symbol-table?]

Creates an immutable, local symbol table. There are no persistent immutable symbol tables.

@defproc[(symbol-table-set [table immutable-symbol-table?]
                           [id identifier?]
                           [v (or/c syntax? syntax-datum?)]) immutable-symbol-table?]

like @racket[free-id-table-set]

@defproc[(symbol-table-remove [table immutable-symbol-table?]
                              [id identifier?]) immutable-symbol-table?]

like @racket[free-id-table-remove]

@subsection{Symbol sets}

@defproc[(symbol-set? [v any/c]) boolean?]

Returns @racket[#t] if @racket[v] is a symbol set, @racket[#f] otherwise.

@defproc[(in-symbol-set [table symbol-set?]) (sequence/c identifier?)]

Like @racket[in-free-id-set].

@defproc[(mutable-symbol-set? [v any/c]) boolean?]

Returns @racket[#t] if @racket[v] is a mutable symbol set, @racket[#f] otherwise.

@defform[(define-persistent-symbol-set id)]

Defines a (mutable) symbol set for global use like @racket[define-persistent-symbol-table].

@defproc[(local-symbol-set [id identifier?] ...) mutable-symbol-set?]

Creates a local (mutable) symbol set containing the given identifiers.

@defproc[(symbol-set-add! [s mutable-symbol-set?] [id identifier?]) void?]

Like @racket[free-id-set-add!]

@defproc[(symbol-set-member? [s mutable-symbol-set?] [id identifier?]) boolean?]

Like @racket[free-id-set-member?]

@defproc[(immutable-symbol-set? [v any/c]) boolean?]

Returns @racket[#t] if @racket[v] is an immutable symbol set, @racket[#f] otherwise.

@defproc[(immutable-symbol-set [id identifier?] ...) immutable-symbol-set?]

Creates a (local) immutable symbol set containing the given identifiers. There are no persistent immutable symbol sets.

@defproc[(symbol-set-add [s immutable-symbol-set?] [id identifier?]) immutable-symbol-set?]

Like @racket[free-id-set-add]

@defproc[(symbol-set-remove [s immutable-symbol-set?] [id identifier?]) immutable-symbol-set?]

Like @racket[free-id-set-remove]

@defproc[(symbol-set-union [s immutable-symbol-set?] ...) immutable-symbol-set?]

Like @racket[free-id-set-union]

@defproc[(symbol-set-intersect [s0 immutable-symbol-set?] [s immutable-symbol-set?] ...) immutable-symbol-set?]

Like @racket[free-id-set-intersect].

@defproc[(symbol-set-subtract [s0 immutable-symbol-set?] [s immutable-symbol-set?] ...) immutable-symbol-set?]

Like @racket[free-id-set-subtract].

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
