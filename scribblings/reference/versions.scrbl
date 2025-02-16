#lang scribble/manual

@(require "../common.rkt" (for-label racket syntax-spec-v3 racket/stxparam))

@title{Release Notes}

This package is periodically released as a package on the package server with a versioned package and collection name, like @hyperlink["https://pkgs.racket-lang.org/package/syntax-spec-dev"]{@tt{syntax-spec-dev}}. The unversioned package name @tt{syntax-spec} is used for the current unstable development version.

Breaking changes may occur between differently-named versions. This page documents the history of breaking changes. Other new features are not mentioned here.

The version used in the paper @hyperlink["https://dl.acm.org/doi/10.1145/3674627"]{"Compiled, Extensible, Multi-language DSLs (Functional Pearl)"} was @tt{syntax-spec-v2}.

@section[#:style '(unnumbered)]{Version 3}

Binding specifications now require ellipses matching the ellipsis depth of pattern variables in the syntax spec. See the  @hyperlink["https://github.com/michaelballantyne/syntax-spec/pull/37]"]{PR description} for more details.

With this change the new @tt{nest} syntax accomplishes the behavior of the old @tt{nest} syntax when the first form is followed by ellipses and the behavior of the old @tt{nest-one} syntax when no ellipses are used.

@tech{Reference compilers} are now specified as part of @tech[#:key "binding classes"]{binding class} declarations, rather than with @tt{with-reference-compilers}. If you previously used @tt{with-reference-compilers} to create reference compilers with contextual behavior, you can typically use @tech/reference{syntax parameters} to accomplish the same with the new design.

@section[#:style '(unnumbered)]{Version 2}

Some forms were renamed:

@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
         (list (list @bold{Old name} @bold{New name})
               (list @tt{recursive} @tt{import})
               (list @tt{nonterminal/two-pass} @tt{nonterminal/exporting}))]

Scopes in binding specifications are now indicated by the @tt{scope} form rather than @tt{{}} curly braces.

Reference compilers are now invoked at application forms like @tt{(x y z)} where @tt{x} is the DSL reference. Use @racket[make-variable-like-reference-compiler] if you only want to transform references in reference or @racket[set!] positions.

