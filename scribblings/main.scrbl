#lang scribble/manual

@(require (for-label racket))

@title{syntax-spec: A Metalanguage for Hosted DSLs}
@;@title{syntax-spec: Declaring DSL syntaxes}
@author+email["Michael Ballantyne" "michael.ballantyne@gmail.com"]


This package provides a metalanguage for creating hosted DSLs. @deftech{Hosted DSLs} extend
the syntax of Racket with their own grammar and have their own static semantics and compilers.

The metalanguage allows programmers to declare a DSL's grammar, binding rules, and integration points
with Racket. Under the hood it produces a macro expander for the DSL that parses, checks name bindings,
expands DSL macros, and produces syntax in the DSL's core language for compilation.

You can implement conventional macros that do all these same things, but it can take a lot of manual
effort and a deep knowledge of Racket's syntax API. 



You might find the metalanguage useful when you both:



@itemlist[
 @item{want to perform static analysis or optimizing compilation}
 @item{you want your DSL to be macro-extensible}
]

@local-table-of-contents[]

@include-section["tutorial/main.scrbl"]

@include-section["reference/main.scrbl"]

