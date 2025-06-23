#lang scribble/manual

@(require "../common.rkt" (for-label racket syntax/parse "../../main.rkt"))


@title{Specifying Languages}

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
@tex-label["doc:binding-classes"]


@deftech{Binding classes} distinguish types of binding. When a reference resolves to a
binder, it is an error if the binding class declared for the reference position
does not match the binding class of the binding position.

@defform[(binding-class id binding-class-option ...)
            #:grammar
            [(binding-class-option (code:line #:description string-literal)
                                   (code:line #:binding-space space-symbol)
                                   (code:line #:reference-compiler reference-compiler-expr))]]

The @racket[#:description] option provides a user-friendly phrase describing the
kind of binding. This description is used in error messages.

The @racket[#:binding-space] option specifies a @tech/reference{binding space}
to use for all bindings and references declared with this class.
@;
Operationally, the binding space declaration causes the syntax-spec expander to
add the binding space scope to bindings and references.
@;
The scope is added to the scope sets of all binding occurrences.
@;
When parsing a reference position declared with a binding
class that has an associated binding space, the name that is looked up is
augmented with the binding class scope in order to give it access to bindings
defined in the space.

@margin-note{See @secref["reference compilers"] for more information about reference compilers}

The @racket[#:reference-compiler] option specifies a @tech{reference compiler} for controlling how
references to variables of this binding class are treated in Racket code.

@section{Extension classes}

@deftech{Extension classes} distinguish types of extensions to languages. A syntax transformer is tagged with an extension
class using @racket[define-dsl-syntax].
Nonterminals can be declared extensible by a certain extension class using @racket[#:allow-extension].
These extensions are expanded away into core DSL forms before compilation.

@defform[(extension-class id extension-class-option)
            #:grammar
            [(extension-class-option (code:line #:description string-literal)
                                     (code:line #:binding-space space-symbol))]]

The @racket[#:description] option provides a user-friendly phrase describing the
kind of extension. This description is used in error messages.

The @racket[#:binding-space] option specifies a @tech/reference{binding space}
to use for all extensions with this class.

@section{Nonterminals}

@defform[(nonterminal id nonterminal-option production ...)]

Defines a nonterminal supporting @racket[let]-like binding structure.

Example:

@(define let-example
    (racketblock
      (syntax-spec
        (binding-class my-var)
        (nonterminal my-expr
          n:number
          x:my-var
          (my-let ([x:my-var e:my-expr] ...) body:my-expr)
          #:binding (scope (bind x) ... body)))))

@let-example

@defform[(nonterminal/nesting id (nested-id) nonterminal-option production ...)]

Defines a @deftech{nesting nonterminal} supporting nested, @racket[let*]-like binding structure. Nesting nonterminals may also be used to describe complex binding structures like for @racket[match].

Example:
@define[let*-example
@racketblock[

(syntax-spec
  (binding-class my-var)
  (nonterminal my-expr
    n:number
    x:my-var
    (my-let* (b:binding-pair ...) body:my-expr)
    #:binding (nest b ... body))
  (nonterminal/nesting binding-pair (nested)
    [x:my-var e:my-expr]
    #:binding (scope (bind x) nested)))
]]
@let*-example

@defform[(nonterminal/exporting id nonterminal-option production ...)]

Defines an @deftech{exporting nonterminal} which can export bindings, like @racket[define] and @racket[begin].

Example:

@racketblock[
(syntax-spec
  (binding-class my-var)
  (nonterminal/exporting my-defn
    (my-define x:my-var e:my-expr)
    #:binding (export x)

    (my-begin d:my-defn ...)
    #:binding [(re-export d) ...])
  (nonterminal my-expr
    n:number))
]

@subsection{Nonterminal options}

@racketgrammar*[(nonterminal-option (code:line #:description string-literal)
                                    (code:line #:allow-extension extension-class-spec)
                                    (code:line #:binding-space space-symbol))
                (extension-class-spec extension-class-id
                                      (extension-class-id ...))]

The @racket[#:description] option provides a user-friendly phrase describing the kind of nonterminal. This description is used in error messages.

The @racket[#:allow-extension] option makes the nonterminal extensible by macros of the given extension class(es).

The @racket[#:binding-space] option specifies a @tech/reference{binding space} to use for all bindings and references declared with this nonterminal.

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

A @deftech{rewrite production} allows certain terms to be re-written into other forms. For example, you might want to tag literals:

@racketblock[
(syntax-spec
  (nonterminal peg
    (~> (~or s:string s:char s:number s:regexp)
        #:with #%peg-datum (datum->syntax #'s '#%peg-datum)
        #'(#%peg-datum s))

    ...))
]

Rewrite productions don't have binding specs since they declare an expansion of @tech{surface syntax} into a another DSL form. The don't necessarily have to expand into a core form like one declared by a form production or a syntax production. A rewrite production can expand into a DSL macro usage or another rewrite production.

Form productions and syntax productions declare core forms in the nonterminal which may have binding specs. If a binding spec is not provided, one is implicitly created. In this case, or if any spec variable is excluded from a binding spec in general, it will be treated as a reference position and implicitly added to the binding spec.

A form production defines a form with the specified name. You may want to use a syntax production if you are re-interpreting racket syntax. For example, if you are implementing your own @racket[block] macro using syntax-spec:

@racketblock[
(syntax-spec
  (nonterminal/exporting block-form
    #:allow-extension racket-macro

    ((~literal define-values) (x:racket-var ...) e:racket-expr)
    #:binding [(export x) ...]

    ((~literal define-syntaxes) (x:racket-macro ...) e:expr)
    #:binding (export-syntaxes x ... e)

    e:racket-expr))
]

When a form production's form is used outside of the context of a syntax-spec DSL, like being used directly in Racket, a syntax error is thrown.

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

@deftech{Syntax specs} declare the grammar of a DSL form.

@itemlist[
@item{
  @racket[()] specifies an empty list.
}
@item{
  @racket[keyword] specifies a keyword like @racket[#:key] .
}
@item{
  @racket[...] specifies zero or more repetitions of the previous syntax spec.
}
@item{
  @racket[...+] specifies one or more repetitions of the previous syntax spec.
}
@item{
  @racket[(~literal id maybe-space)] specifies a literal identifier and optionally allows the specification of a @tech/reference{binding space} for the identifier.
}
@item{
  @racket[(~datum id)] specifies a datum literal.
}
@item{
  @racket[(syntax-spec . syntax-spec)] specifies a pair of syntax specs.
}
@item{
  @racket[spec-variable-id:binding-class-id] specifies an identifier sub-expression belonging to the specified binding class. For example: @racket[x:my-var] specifies an identifier of the @racket[my-var] binding class.
}
@item{
  @racket[spec-variable-id:nonterminal-id] specifies a sub-expression that conforms to the specified nonterminal's grammar. For example: @racket[e:my-expr] specifies a sub-expression that is a @racket[my-expr], where @racket[my-expr] is a nonterminal name.
}
@item{
  @racket[spec-variable-id:extension-class-id] specifies a sub-expression that is treated as a phase-1 transformer expression. This is used when your language has macro definitions.
}
]

@section{Binding specs}
@tex-label{doc:bindingspecs}


@racketgrammar*[#:literals (bind bind-syntax bind-syntaxes import re-export export export-syntax export-syntaxes nest ...)
               (binding-spec
                 spec-variable-id
                 (bind spec-variable-id)
                 (bind-syntax spec-variable-id spec-variable-id)
                 (bind-syntaxes spec-variable-id ooo ... spec-variable-id)
                 (scope spec-or-ooo ...)
                 [spec-or-ooo ...]
                 (nest spec-variable-id ooo ... binding-spec)
                 (import spec-variable-id)
                 (export spec-variable-id)
                 (export-syntax spec-variable-id spec-variable-id)
                 (export-syntaxes spec-variable-id ooo ... spec-variable-id)
                 (re-export spec-variable-id))
               (ooo
                 ...)
               (spec-or-ooo
                 binding-spec
                 ooo)]

@deftech{Binding specs} declare the binding rules of a DSL's forms. They allow us to control the scope of bound variables and to check that programs are well-bound before compilation. A binding spec is associated with a production and refers to spec variables from the production.

Similar to syntax patterns and templates, syntax specs and binding specs have a notion of ellipsis depth. However, all spec references in binding specs must have the exact same ellipsis depth as their syntax spec counterparts. Ellipses in binding specs are used to declare the scoping structure of syntax that includes sequences.

@itemlist[
@item{
  For a @racket[spec-variable-id] binding spec, if the specified sub-expression is an identifier (the syntax spec would be something like @racket[x:my-var]), then the identifier is treated as a reference. If the specified sub-expression is something else (the syntax spec would be something like @racket[x:my-expr]), then the resulting binding structure depends on the content of the sub-expression. If a sub-expression of a production is excluded from its binding spec it is implicitly added as this type of binding spec.
}
@item{
  @racket[(bind x)] declares that the variable specified by @racket[x] is bound in the current scope. @racket[bind] must be used inside of a @racket[scope] and @racket[x] must be specified with a binding class in its syntax spec like @racket[x:my-var].

  Example:
  @let-example

  Notice how there are ellipses after the @racket[(bind x)] since @racket[x] occurred inside of an ellipsized syntax spec.
}
@item{
  @racket[(bind-syntax x e)] declares that the variable specified by @racket[x] is bound to the transformer specified by @racket[e]. @racket[bind-syntax] must be used inside of a @racket[scope], @racket[x] must be specified with a binding class in its syntax spec like @racket[x:my-var], and @racket[e] must be specified with an extension class in its syntax spec like @racket[e:my-macro].

  Example:
  @racketblock[
  (syntax-spec
    (binding-class my-var)
    (extension-class my-macro)
    (nonterminal my-expr
      #:allow-extension my-macro
      n:number
      (my-let-syntax ([x:my-var trans:my-macro]) body:my-expr)
      #:binding (scope (bind-syntax x trans) body)))
  ]
}
@item{
  @racket[bind-syntaxes] is similar to @racket[bind-syntax], except it binds multiple identifiers.

  Example:
  @racketblock[
  (syntax-spec
    (binding-class my-var)
    (extension-class my-macro)
    (nonterminal my-expr
      #:allow-extension my-macro
      n:number
      (my-let-syntaxes ([(x:my-var ...) trans:my-macro]) body:my-expr)
      #:binding (scope (bind-syntaxes x ... trans) body)))
  ]
  Here, @racket[trans] should evaluate to multiple transformers using @racket[values].

  Note that the ellipses for @racket[x] occur inside of the @racket[bind-syntaxes].
}
@item{
  @racket[scope] declares that bindings and sub-expressions in the sub-specs are in a particular scope. Local bindings binding specs like @racket[bind] must occur directly in a @racket[scope] binding spec.
}
@item{
  @racket[[spec ...]] is called a group, and it groups binding specs together. For example, when we write
  @racketblock[
    (my-let ([x:my-var e:my-expr]) body:my-expr)
    #:binding (scope (bind x) e)
  ]
  We could instead write
  @racketblock[
    (my-let ([x:my-var e:my-expr]) body:my-expr)
    #:binding [(scope (bind x) body) e]
  ]
  Which adds that @racket[e] is a sub-expression outside of the scope of the @racket[let]. All un-referenced syntax spec variables get implicitly added to a group with the provided binding spec, so the former example is equivalent to the latter.

  Ellipses can occur after a binding spec in a group.
}
@item{
  @racket[nest] is used with @tech{nesting nonterminals}. In particular, the first argument to nest must be a spec variable associated with a nesting nonterminal. The second argument is treated as the "base case" of the "fold".

  Example:

  @let*-example

  The @racket[nest] binding spec sort of folds over the binding pairs. In this example, it'll produce binding structure like

  @racketblock[[e1 (scope (bind x1) [e2 (scope (bind x2) [... [en (scope (bind xn) body)]])])]]

  @racket[nest] does not necessarily have to be used with a sequence.

  Example:
  @racketblock[
    (syntax-spec
      (binding-class pattern-var)
      (nonterminal clause
        [p:pat e:racket-expr]
        #:binding (nest p e))
      (nonterminal/nesting pat (nested)
        x:pattern-var
        #:binding (scope (bind x) nested)
        ((~literal cons) car-pat:pat cdr-pat:pat)
        #:binding (nest car-pat (nest cdr-pat nested))))
  ]

  However, the first arguemnt of @racket[nest] cannot have ellipsis depth exceeding one.
}
@item{
  @racket[(import d)] imports the bindings exported from the sub-expression specified by @racket[d]. @racket[import] must be used inside of a @racket[scope] and must refer to a syntax spec associated with an @tech{exporting nonterminal}.

  Example:
  @racketblock[
  (syntax-spec
    (binding-class my-var)
    (nonterminal/exporting my-defn
      (my-define x:my-var e:my-expr)
      #:binding (export x)

      (my-begin d:my-defn ...)
      #:binding [(re-export d) ...])
    (nonterminal my-expr
      n:number
      (my-local [d:my-defn ...] body:my-expr)
      #:binding (scope (import d) ... body)))
  ]

  The argument to @racket[import] cannot have ellipsis depth exceeding one.
}
@item{
  @racket[(export x)] exports the variable specified by @racket[x]. @racket[x] must refer to a syntax spec variable associated with a binding class and @racket[export] can only be used in an @tech{exporting nonterminal}. See @racket[import] for an example of usage.
}
@item{
  @racket[export-syntax] is like @racket[bind-syntax], except it exports the binding instead of binding the identifier locally to the current scope. Like @racket[export], it can only be used in an @tech{exporting nonterminal}.
}
@item{
  @racket[export-syntaxes] is like @racket[bind-syntaxes], except it exports the bindings instead of binding the identifiers locally to the current scope. Like @racket[export], it can only be used in an @tech{exporting nonterminal}.
}
@item{
  @racket[(re-export d)] @racket[export]s all bindings that are exported by @racket[d]. @racket[d] must be associated with an @tech{exporting nonterminal} and @racket[re-export] can only be used in an @tech{exporting nonterminal}. See @racket[import] for an example of usage.
}
]

There are several other constraints on binding specs:

@itemlist[
@item{
  Specs of different categories cannot occur within the same ellipsis. The categories of specs are:
  @itemlist[
    @item{
      @racket[refs+subexps] include references, @racket[nest], and @racket[scope].
    }
    @item{
      @racket[binds] include @racket[bind], @racket[bind-syntax], and @racket[bind-syntaxes].
    }
    @item{
      @racket[imports] include @racket[import].
    }
    @item{
      @racket[exports] include @racket[export], @racket[export-syntax], @racket[export-syntaxes], and @racket[re-export].
    }
  ]
  For example, the spec @racket[(scope [(bind x) e] ...)] is illegal since it mixes @racket[refs+subexps] and @racket[binds] in an ellipsis.
}
@item{
  @racket[binds] and @racket[imports] can only occur within a @racket[scope]
}
@item{
  @racket[exports] cannot occur within a scope.
}
@item{
  Within a scope, there can be zero or more @racket[binds], followed by zero or more @racket[imports], followed by zero or more @racket[refs+subexps].
}
@item{
  The second argument to nest must be @racket[refs+subexps].
}
@item{
  Spec variables can be used at most once. For example, @racket[(scope (bind x) e e)] is illegal.
}
]

@section{Host interface forms}

Host interface forms are the entry point to the DSL from the host language. They often invoke a compiler macro to translate
the DSL forms into Racket expressions.

@defform[(host-interface/expression
              (id . syntax-spec)
              maybe-binding-spec
              pattern-directive ...
              body ...+)]

Defines a host interface to be used in expression positions.

Can only be used inside of a @racket[syntax-spec] block.

An example from the @hyperlink["https://github.com/michaelballantyne/syntax-spec/blob/b19d995f7fd8418ef2f867df9cdaff6283ca7280/tests/dsls/minikanren-rs2e/mk.rkt#L146-L152"]{miniKanren DSL}:

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

@defform[#:literals (-> define)
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

An example from the @hyperlink["https://github.com/michaelballantyne/syntax-spec/blob/b19d995f7fd8418ef2f867df9cdaff6283ca7280/tests/dsls/minikanren-rs2e/mk.rkt#L129-L144"]{miniKanren DSL}:

@racketblock[
(syntax-spec
  ...

  (host-interface/definition
    (defrel (name:relation-name x:term-variable ...) g:goal)
    #:binding [(export name) (scope (bind x) ... g)]

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

@defform[(host-interface/definitions
              (id . syntax-spec)
              maybe-binding-spec
              pattern-directive ...
              body ...+)]

Defines a host interface to be used in a definition context.

Can be used to produce multiple definitions.

Can only be used inside of a @racket[syntax-spec] block.

An example from the @hyperlink["https://github.com/michaelballantyne/syntax-spec/blob/b19d995f7fd8418ef2f867df9cdaff6283ca7280/tests/dsls/peg/core.rkt#L113-L119"]{PEG DSL}:

@racketblock[
(syntax-spec
  (host-interface/definitions
   (define-pegs [name:nonterm p:peg] ...)
   #:binding [(export name) ...]
   (run-leftrec-check! (attribute name) (attribute p))
   #'(begin (define name (lambda (in) (compile-peg p in)))
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

A nonterminal that allows arbitrary host language expressions. Such @deftech{host expressions} are wrapped with @racket[#%host-expression] during DSL expansion. This nonterminal does not support definitions.

@defidform[#:kind "nonterminal" racket-body]

A nonterminal that allows arbitrary host language expressions and definitions. This is an exporting nonterminal, so it must be explicitly mentioned in a binding spec, usually with @racket[import].

Example:

@racketblock[
(syntax-spec
  (nonterminal my-expr
    (my-let ([x:racket-var e:racket-expr]) body:racket-body ...+)
    #:binding (scope (bind x) (import body) ...)))
]

@defidform[#:kind "binding class" racket-var]

A binding class for host language bindings.

@defidform[#:kind "extension class" racket-macro]

A binding class for arbitrary host language transformers.
