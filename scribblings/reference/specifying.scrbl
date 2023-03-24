#lang scribble/manual

@(require (for-label racket syntax-spec))

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

@defsubform[(extension-class id maybe-description maybe-binding-space)
            #:grammar
            [(maybe-description (code:line #:description string-literal)
                                (code:line))
             (maybe-binding-space (code:line #:binding-space space-symbol)
                                  (code:line))]]
@section{Nonterminals}

@defsubform[(nonterminal id nonterminal-options production ...)]
@defsubform[(nonterminal/nesting id (nested-id) nonterminal-options production ...)]
@defsubform[(nonterminal/two-pass id nonterminal-options production ...)]

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


@racketgrammar[#:literals (bind recursive re-export export nest nest-one)
               binding-spec spec-variable-id
               (bind spec-variable-id ...+)
               {spec ...}
               [spec ...]
               (nest spec-variable-id binding-spec)
               (nest-one spec-variable-id binding-spec)
               (recursive spec-variable-id ...+)
               (export spec-variable-id ...+)
               (re-export spec-variable-id ...+)]

@section{Defining host interface forms}

@defsubform[(host-interface/expression
              (id . syntax-spec)
              maybe-binding-spec
              pattern-directive ...
              body ...+)]

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

@defsubform[(host-interface/definitions
              (id . syntax-spec)
              maybe-binding-spec
              pattern-directive ...
              body ...+)]

@section{Embedding Racket syntax}

@defidform[#:kind "nonterminal" racket-expr]

@defidform[#:kind "binding class" racket-var]

@defidform[#:kind "extension class" racket-macro]

