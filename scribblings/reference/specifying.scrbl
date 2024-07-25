#lang scribble/manual

@(require (for-label racket syntax/parse "../../main.rkt"))

@;-----------------------

@(define (tech/reference str)
   (tech #:doc '(lib "scribblings/reference/reference.scrbl") str))

@(define (seclink/reference sec str)
   (seclink sec #:doc '(lib "scribblings/reference/reference.scrbl") str))

@;-----------------------


@title{Specifying languages}


This section describes the syntax of the @racket[syntax-spec] metalanguage,
used to describe the grammar, binding structure, and host interface of a DSL.

Language specifications are made via the subforms of @racket[syntax-spec],
which must be used at module-level.

@defform[(syntax-spec spec-def ...)
         #:grammar
         [(spec-def binding-class
                    extension-class
                    nonterminal
                    host-interface)]]

The following subsections address each kind of declaration allowed within the
@racket[syntax-spec] form.



@section{Binding classes}

Binding classes distinguish types of binding. When a reference resolves to a
binder, it is an error if the binding class declared for the reference position
does not match the binding class of the binding position.

@defsubform[(binding-class id maybe-description maybe-binding-space)
            #:grammar
            [(maybe-description (code:line #:description string-literal)
                                (code:line))
             (maybe-binding-space (code:line #:binding-space space-symbol)
                                  (code:line))]]

The @racket[#:description] option provides a user-friendly phrase describing the
kind of binding. This description is used in error messages.

The @racket[#:binding-space] option specifies a @tech/reference{binding space}
to use for all bindings and references declared with this class.

Operationally, the binding space declaration causes the syntax-spec expander to
add the binding space scope to bindings and references.
@;
The scope is added to the scope sets of all binding occurrences.
@;
When parsing a reference position declared with a binding
class that has an associated binding space, the name that is looked up is
augmented with the binding class scope in order to give it access to bindings
defined in the space.

@section{Extension classes}

@deftech{Extension classes} distinguish types of extensions to languages. A syntax transformer is tagged with an extension
class using @racket[define-dsl-syntax].
Nonterminals can be declared extensible by a certain extension class using @racket[#:allow-extension].
These extensions are expanded away into core DSL forms before compilation.

@defsubform[(extension-class id maybe-description maybe-binding-space)
            #:grammar
            [(maybe-description (code:line #:description string-literal)
                                (code:line))
             (maybe-binding-space (code:line #:binding-space space-symbol)
                                  (code:line))]]

The @racket[#:description] option provides a user-friendly phrase describing the
kind of extension. This description is used in error messages.

The @racket[#:binding-space] option specifies a @tech/reference{binding space}
to use for all extensions with this class.

@section{Nonterminals}

@defsubform[(nonterminal id nonterminal-options production ...)]
@defsubform[(nonterminal/nesting id (nested-id) nonterminal-options production ...)]
@defsubform[(nonterminal/exporting id nonterminal-options production ...)]

@subsection{Nonterminal options}

@racketgrammar*[(nonterminal-options (code:line maybe-description #;maybe-bind-literal-set
                                                maybe-allow-extension
                                                maybe-binding-space))
                (maybe-description (code:line #:description string-literal)
                                   (code:line))
                #;(maybe-bind-literal-set (code:line #:bind-literal-set literal-set-id)
                                        (code:line))
                (maybe-allow-extension (code:line #:allow-extension extension-class-spec)
                                       (code:line))
                (extension-class-spec extension-class-id
                                      (extension-class-id ...))
                (maybe-binding-space (code:line #:binding-space space-symbol)
                                     (code:line))]

@subsection{Productions}

@racketgrammar*[#:literals (~>)
                (production rewrite-production
                            form-production
                            syntax-production)
                (rewrite-production (~> syntax-pattern
                                        pattern-directive ...
                                        body ...+))
                (form-production (code:line (form-id . syntax-spec) maybe-binding-spec)
                                 form-id)
                (syntax-production (code:line syntax-spec maybe-binding-spec))
                (maybe-binding-spec (code:line #:binding binding-spec)
                                    (code:line))]

@section{Syntax specs}

@racketgrammar*[#:literals (~> ~literal ~datum ... ...+)
                (syntax-spec ()
                             keyword
                             ...
                             ...+
                             (~literal id maybe-space)
                             (~datum id)
                             (syntax-spec . syntax-spec)
                             spec-variable-id:binding-class-id
                             spec-variable-id:nonterminal-id
                             spec-variable-id:extension-class-id
                             #;spec-variable-id:syntax-class-id)
                (maybe-space (code:line #:space space-name)
                             (code:line))]


@section{Binding specs}


@racketgrammar[#:literals (bind bind-syntax bind-syntaxes import re-export export export-syntax export-syntaxes nest nest-one)
               binding-spec spec-variable-id
               (bind spec-variable-id ...+)
               (bind-syntax spec-variable-id spec-variable-id)
               (bind-syntaxes spec-variable-id spec-variable-id)
               (scope spec ...)
               [spec ...]
               (nest spec-variable-id binding-spec)
               (nest-one spec-variable-id binding-spec)
               (import spec-variable-id ...+)
               (export spec-variable-id ...+)
               (export-syntax spec-variable-id spec-variable-id)
               (export-syntaxes spec-variable-id spec-variable-id)
               (re-export spec-variable-id ...+)]

@section{Defining host interface forms}

Host interface forms are the entry point to the DSL from the host language. They often invoke a compiler macro to translate
the DSL forms into Racket expressions.

@defsubform[(host-interface/expression
              (id . syntax-spec)
              maybe-binding-spec
              pattern-directive ...
              body ...+)]

Defines a host interface to be used in expression positions.

Can only be used inside of a @racket[syntax-spec] block.

An example from the miniKanren DSL:

@racketblock[
(syntax-spec
  ...

  (host-interface/expression
    (run n:expr q:term-variable g:goal)
    #:binding (scope (bind q) g)

    #`(let ([q (var 'q)])
        (map (reify q)
             (run-goal n (compile-goal g))))))
]

This defines @racket[run], which takes in a Racket expression representing a number, a term variable, and a goal, and invokes
the compiler @racket[compile-goal] to translate the DSL forms into Racket.

@defsubform[#:literals (-> define)
            (host-interface/definition
              (id . syntax-spec)
              maybe-binding-spec
              #:lhs
              [pattern-directive ...
               body ...+]
              #:rhs
              [pattern-directive ...
               body ...+])]

Defines a host interface to be used in a definition context.

@racket[#:lhs] runs before the right-hand-sides of definitions in the current context expand and must produce the identifier being defined.

@racket[#:rhs] runs after the left-hand-sides of definitions and must produce the Racket expression whose value will be
bound to the identifier (don't emit the definition syntax, just the syntax for producing the value).

Can only be used inside of a @racket[syntax-spec] block.

An example from the miniKanren DSL:

@racketblock[
(syntax-spec
  ...

  (host-interface/definition
    (defrel (name:relation-name x:term-variable ...) g:goal)
    #:binding [(export name) (scope (bind x) g)]

    #:lhs
    [(symbol-table-set!
      relation-arity
      #'name
      (length (syntax->list #'(x ...))))
     #'name]

    #:rhs
    [#`(lambda (x ...)
         (lambda (s)
           (lambda ()
             (#%app (compile-goal g) s))))]))
]

This defines @racket[defrel], which defines a relation. In the @racket[#:lhs], We record arity information about the identifier before producing it. Since the left-hand-sides all run before the right-hand-sides, even if there is mutual recursion, all arity information will be available before any goals are compiled. Note that the @racket[#:rhs] produces a lambda expression, not a @racket[define].

@defsubform[(host-interface/definitions
              (id . syntax-spec)
              maybe-binding-spec
              pattern-directive ...
              body ...+)]

Defines a host interface to be used in a definition context.

Can be used to produce multiple definitions.

Can only be used inside of a @racket[syntax-spec] block.

An example from the PEG DSL:

@racketblock[
(syntax-spec
  (host-interface/definitions
   (define-pegs [name:nonterm p:peg] ...)
   #:binding (export name)
   (run-leftrec-check! (attribute name) (attribute p))
   #'(begin (define name (lambda (in) (with-reference-compilers ([var immutable-reference-compiler])
                                        (compile-peg p in))))
            ...)))
]

Unlike @racket[host-interface/definition], the definitions are directly produced by the host interface.

@section{Defining macros for DSLs}

@defform[(define-dsl-syntax name extension-class-id transformer-expr)]

Defines a macro of the specified extension class. The transformer expression can be any Racket expression that evaluates to a @racket[(-> syntax? syntax?)] procedure, so it is possible to use @racket[syntax-rules], @racket[syntax-case], @racket[syntax-parse], etc.

Example:

@racketblock[
(define-dsl-syntax conj goal-macro
  (syntax-parser
    [(_ g) #'g]
    [(_ g1 g2 g* ...) #'(conj (conj2 g1 g2) g* ...)]))
]

This defines a macro @racket[conj] that expands to a goal in miniKanren.

@section{Embedding Racket syntax}

@defidform[#:kind "nonterminal" racket-expr]

A nonterminal that allows arbitrary host language expressions. Expressions are wrapped with @racket[#%host-expression] during DSL expansion.

@defidform[#:kind "binding class" racket-var]

A binding class for host language bindings.

@defidform[#:kind "extension class" racket-macro]

A binding class for arbitrary host language transformers.
