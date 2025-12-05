#lang scribble/manual

@(require (for-label racket racket/match racket/list syntax/parse "../../main.rkt" ocular-patdown)
          scribble/example
          racket/sandbox)
@(define eval (make-base-eval '(require racket (for-syntax racket) syntax-spec-dev)))
@(define op-eval (make-base-eval '(require racket (for-syntax racket) ocular-patdown/update ocular-patdown)))
@(define-syntax-rule (repl body ...) (examples #:eval eval #:label #f body ...))
@(define-syntax-rule (op-repl body ...) (examples #:eval op-eval #:label #f body ...))

@title[#:tag "ocular-patdown"]{Advanced Tutorial: A Match-Like DSL for Deep Immutable Updates}

This tutorial outlines the implementation of a DSL that uses lenses and pattern matching to perform deep immutable updates.

We will rebuild a simplified version of the @racket[update] DSL from the @racketmodname[ocular-patdown] package.

@section{Preview}

Here is the language we will be implementing:

@op-repl[
(define lst (list 1 2 3 4))
(update lst
  [(list a b c ...)
   (set! a 'one)
   (modify! c add1)])
lst
]

It's like @racket[match], but we can use @racket[set!] to perform immutable updates on the matched value. This allows us to write code as if we're mutating, but we don't actually affect the matched value.

The original @racket[lst] value is unchanged, but @racket[update] returns a new copy of the list with the changes applied.

@section{Lenses}

This language's runtime is powered by a tool called lenses. Lenses are essentially a first-class, immutable getter and setter for a part of a structure.

Here is an example:

@op-repl[
(define car-lens
  (make-lens car (code:comment "getter")
             (lambda (pair new-car) (code:comment "setter")
               (cons new-car (cdr pair)))))
(define pair (cons 1 2))
(lens-get car-lens pair)
(lens-set car-lens pair 'one)
pair
]

We can also make lenses for working with @racket[struct] fields.

@op-repl[
(struct posn [x y] #:transparent)
(define posn-x-lens (struct-lens posn x))
(lens-set posn-x-lens
          (posn 1 2)
          10)
]

These examples could just as easily be accomplished using @racket[struct-copy] or @racket[match], but lenses can be composed for deep updates, which is where they become more useful.

@op-repl[
(define first-posn-x-lens
  (lens-compose car-lens posn-x-lens))
(lens-set first-posn-x-lens
          (list (posn 1 2) (posn 3 4))
          10)
]

We also have traversals, which are useful for collections like lists.

@op-repl[
(traversal-map list-traversal
                  (list 1 2 3 4)
                  add1)
]

The umbrella term for lenses and traversals is optics.

Traversals can be composed with other optics to "map" them over a collection.

@op-repl[
(define posns-xs-traversal
  (optic-compose list-traversal
                 posn-x-lens))

(traversal-map posns-xs-traversal
               (list (posn 1 2) (posn 3 4))
               -)
]

Optics are useful for implementing @racket[update] because they allow us to retrieve and immutable update values deep in a data structure.

@section{Compilation Preview}

The big idea of this language's implementation is to bind pattern variables to optics, and use a runtime parameter to keep track of the current version of the target. Here is a simplified example compilation:

@(define compilation-preview-example
(racketblock
(update pair
  [(cons a _)
   (set! a (add1 a))])
~>
(let ([a car-lens])
  (parameterize ([current-update-target pair])
    (current-update-target
     (lens-set a
               (current-update-target)
               (add1 (lens-get a (current-update-target)))))
    (current-update-target)))
))
@compilation-preview-example

Patterns get translated to @racket[let] and optics. Variable references get translated to @racket[lens-get], @racket[set!] gets translated to @racket[lens-set], and they both use the parameter @racket[current-update-target]. The result of the @racket[set!] is the new value of the target.

@section{The Expander}

@subsection{Syntactic Sugar}

Some patterns are equivalent to others. For example:

@racketblock[
(list a b c)
~>
(cons a (cons b (cons c _)))
]

For the purpose of this tutorial, we will only allow a single clause in @racket[update] and assume that the pattern is appropriate for the target. The real implementation supports multiple clauses and checks that patterns match, but this tutorial follows a simplified version of the DSL. Thus, we will use a wildcard pattern for the tail of the list, ignoring the possibility of there being more elements.

We will use DSL macros for patterns like @racket[list] which can be expressed in terms of simpler ones.

There are also patterns that aren't equivalent to each other, but compile to very similar code. Consider these two examples:

@racketblock[
(update pair
  [(cons a _)
   <body>])
~>
(let ([a car-lens])
  <compiled-body>)

(update pair
  [(posn x _)
   <body>])
~>
(let ([x (struct-lens posn x)])
  <compiled-body>)
]

In both examples, we're simply binding an optic to a pattern variable. The compilations are almost identical. We can create a general @racket[optic] pattern to capture this idea:

@racketblock[
(cons a _)
~>
(optic car-lens a)

(update <target>
  [(optic <opt> a) <body>])
~>
(let ([a <opt>])
  <compiled-body>)
]

Let's think about what should happen if we have multiple sub-patterns:

@racketblock[
(update pair
  [(cons a d)
   <body>])
~>
(let ([a car-lens])
  (let ([d cdr-lens])
    <compiled-body>))
]

We should produce nested uses of @racket[let]. It's kind of like we're doing both @racket[(optic car-lens a)] and @racket[(optic cdr-lens d)] on @racket[pair]. We can create a general @racket[and] pattern to capture this idea of matching multiple patterns on the same target:

@racketblock[
(cons a d)
~>
(and (optic car-lens a)
     (optic cdr-lens d))
]

Since a lot of the complexity is in the procedural implementation of lenses, the language itself ends up being simple.

@subsection{Core Grammar}

@racket[optic] and @racket[and] are so general, we actually don't need any other core forms! All other patterns we need can be expressed in terms of them.

@racketgrammar[#:literals (optic and2)
pat
id
_
(optic optic-expr pat)
(and2 pat pat)
]

(@racket[and] can be sugar on top of @racket[and2])

There we have it. Now we have to implement it with @racket[syntax-spec].

@racketblock[
(require syntax-spec-dev (for-syntax syntax/parse syntax/parse/class/struct-id))
(syntax-spec
  (binding-class optic-var)
  (extension-class pattern-macro
    #:binding-space pattern-update)
  (nonterminal/exporting pat
    #:allow-extension pattern-macro
    #:binding-space pattern-update
    (~> (name:struct-id field ...)
        #'(struct* name field ...))
    _
    v:optic-var
    #:binding (export v)
    (optic o:racket-expr p:pat)
    #:binding (re-export p)
    (and2 p1:pat p2:pat)
    #:binding [(re-export p1) (re-export p2)]))
]

We define a binding class for pattern variables, an extension class for pattern macros, and a nonterminal for patterns including productions for our core forms.

We use an exporting nonterminal because all pattern variables are in scope in the body, and we don't want to allow duplicates.

We have a special rewrite rule for struct names since we can't use productions or DSL macros for all possible struct names. The pattern macro @racket[struct*] will expand to uses of @racket[and], @racket[optic], and @racket[struct-lens].

Now that we've defined our core grammar and extensibility, let's implement some of our syntactic sugar:

@racketblock[
(define-dsl-syntax cons pattern-macro
  (syntax-parser
    [(_ a d)
     #'(and (optic car-lens a)
            (optic cdr-lens d))]))

(define-dsl-syntax list pattern-macro
  (syntax-parser
    [(_)
     #'_]
    [(_ p (~datum ...))
     #'(optic list-traversal p)]
    [(_ p0 p ...)
     #'(cons p0 (list p ...))]))

(define-dsl-syntax and pattern-macro
  (syntax-parser
    [(_)
     #'_]
    [(_ p0 p ...)
     #'(and2 p0 (and p ...))]))
]

We are defining DSL macros that have the same name as built-in forms and procedures from Racket. However, since we're using binding spaces, we aren't actually shadowing these built-in names.

Now that we have our core grammar, expander, and some convenient syntactic sugar, we're ready to implement the compiler.

@section{The Compiler}

The compiler has two main pieces: The pattern compiler and the body "compiler". The pattern compiler translates patterns into @racket[let] and optics, and the body "compiler" establishes the parameter and transforms variable usages. The body "compiler" isn't a compiler in the same sense as the pattern compiler since it's really just customizing the expansion of the body, which is a Racket expression, or several Racket expressions. We want to support arbitrary Racket expressions to allow for maximum flexibility in the body.

The clause body is an example of a multi-language boundary between our update language and Racket. The body is under a special context introduced by update, hence the need for the body compiler to establish that context.

@subsection{The Pattern Compiler}

We've seen a few examples of pattern compilation. Here is a refresher:

@racket[optic] patterns get translated to a @racket[let].

@racketblock[
(update pair
  [(optic car-lens a) <body>])
~>
(let ([a car-lens])
  <body>)
]

We're ignoring body compilation for now.

But what happens when we have nested patterns?

@racketblock[
(update posns
  [(cons (posn x _) _)
   <body>])
~>
(let ([tmp car-lens])
  (let ([x (optic-compose tmp (struct-lens posn x))])
    <body>))
]

We create temporary variables for parent patterns and use optic composition to "drill down" and create an optic that focuses on a field of a field.

Let's also remind ourselves how compilation of @racket[and] patterns work while we're at it:

@racketblock[
(update posns
  [(cons a d) <body>])
~>
(update posns
  [(and (optic car-lens a)
        (optic cdr-lens b))
   <body>])
~>
(let ([a car-lens])
  (let ([b cdr-lens])
    <body>))
]

We end up with nested uses of @racket[let]. Bindings from the left pattern come before those of the right pattern. If we had more complicated subpatterns, all the left pattern's bindings would come first. For example:

@racketblock[
(update
  [(and (cons a b)
        (cons c d))
   <body>])
~>
(let ([a car-lens])
  (let ([b cdr-lens])
    (let ([c car-lens])
      (let ([d cdr-lens])
        <body>))))
]

Now let's actually implement this:

@racketblock[
(define-syntax bind-optics
  (syntax-parser
    [(_ current-optic:id p body)
     (syntax-parse #'p
       #:datum-literals (_ optic and2)
       [_ #'body]
       [var:id
        #'(let ([var current-optic])
            body)]
       [(optic o p)
        #'(let ([tmp-optic (optic-compose current-optic o)])
            (bind-optics tmp-optic p body))]
       [(and2 p1 p2)
        #'(bind-optics current-optic p1
            (bind-optics current-optic p2 body))])]))
]

Our compiler @racket[bind-optics] takes in the current optic variable name, the pattern to compile, and the body.

@racket[current-optic] is like @racket[tmp] in our earlier example. It refers to an optic that focuses on the part of the structure that this pattern is matching on. As we compile nested patterns, we'll compose optics from sub-patterns with this current optic to drill down further.

One important invariant is that all of the variables of @racket[p] will be bound in @racket[body].

Another invariant is that The pattern @racket[p]'s "sub-target" is the focus of the optic referred to by @racket[current-optic]. This ensures that our compositions are valid and focus on the correct part of the overall target.

The wildcard pattern does not bind anything and simply emits the body.

The variable pattern binds the variable to the current optic, which focuses on that part of the overall target.

The @racket[optic] pattern creates a temporary variable, composes the provided optic with the current one, and recurs on the sub-pattern with the temporary variable as the new current optic. This ensures that the second invariant holds.

The @racket[and2] pattern recurs on both sub-patterns, providing the second sub-pattern's compilation as the body of the first one's. This leads to "all the bindings from the left, then all the bindings from the right" and ensures that the first invariant holds.

Notice that @racket[and2] recurs with the same @racket[current-optic] for each sub-pattern because each sub-pattern is "running" on the same piece of the overall target. This ensures that the second invariant holds.

Now that we have pattern variables being bound to their corresponding optics, we are ready to compile the body so they can be used.

@subsection{The Body Compiler}

You might have noticed that the pattern compiler has absolutely no mention of the target value. This is because variables are bound to optics, which can be used with any target value. In a sense, a pattern is merely a specification of a tree of optic compositions with variables at the leaves.

The body compiler will connect the variables to the target value using a parameter and reference compilers.

Recall the example from the compiler preview:

@compilation-preview-example

The parameter @racket[current-update-target] keeps track of the current value for the target, and forms like @racket[set!] mutate this parameter with the result of immutable updates.

First, let's implement the host interface which will establish this parameter and invoke our pattern compiler:

@racketblock[
(define current-update-target (make-parameter #f))
(syntax-spec
 (host-interface/expression
   (update target:racket-expr
           [p:pat body:racket-expr])
   #:binding (scope (import p) body)
   #'(let ([target-v target])
       (bind-optics identity-iso p
         (parameterize ([current-update-target
                         target-v])
           body)))))
]

We create a temporary variable for the target to avoid duplicate evaluation, use @racket[bind-optics], and wrap the body with a @racket[parameterize] to establish @racket[current-update-target].

Our binding spec declares that all bindings exported from @racket[p] are in scope in @racket[body].

We initialize @racket[current-optic] to @racket[identity-iso]. You don't need to know that that is, just know that it's the identity of optic composition, so composing it with something like @racket[car-lens] is equivalent to just @racket[car-lens].

Now that we establish the parameter, let's transform variable usages with a reference compiler that uses the parameter:

@racketblock[
(define optic-var-reference-compiler
  (make-variable-like-reference-compiler
   (syntax-parser
     [x:id #'(lens-get x (current-update-target))])
   (syntax-parser
     [(set! x val)
      #'(begin
          (current-update-target
           (lens-set x (current-update-target) val))
          (current-update-target))])))

(syntax-spec
  (binding-class optic-var #:reference-compiler optic-var-reference-compiler)
  ...)
]

We must also set the reference compiler in our binding class declaration.

Variable references turn into @racket[lens-get] and @racket[set!] turn into @racket[lens-set]. The result of @racket[set!] is the new value of the target. This is unlike Racket's usual @racket[set!], which returns void. And again, the only mutation is of the parameter, the actual value of the target itself is not being mutated.

This is an interesting example of a reference compiler. The pattern compiler binds pattern variables to optics without reference to the target value. The reference compiler then transforms uses of the pattern variable to apply the corresponding optic to the @racket[current-update-target]. Reference compilers like @racket[immutable-reference-compiler] simply restrict how DSL-bound variables can be used, but this reference compiler completely alters the behavior of DSL-bound variables.

We can add even more custom variable behavior. For example, what if instead of getting the value that a pattern variable refers to, we want its optic directly? Somehow, we'd have to prevent the reference compiler from transforming the variable reference to a @racket[lens-get]. We can do this by adding another host interface:

@racketblock[
(syntax-spec
  (host-interface/expression
    (optic x:pattern-var)
    #'x))
]

The @racket[optic] host interface simply expands to the raw variable reference. Importantly, this will not be transformed by the reference compiler. This is a nice little trick for special variable behaviors.

We can use the @racket[optic] form to add other forms similar to @racket[set!]:

@racketblock[
(define-syntax-rule (modify! x func)
  (begin
    (current-update-target
     (traversal-modify (optic x)
                       (current-update-target)
                       func))
    (current-update-target)))
]

@op-repl[
(update (list 1 2 3)
  [(list a ...)
   (modify! a add1)])
]

And that's it! We can now use this DSL to perform deep immutable udpates on structures with the convenience and clarity of patterns.

To summarize some key points:

@itemlist[
@item{We created a @racket[syntax-spec] frontend around a procedural library for optics to make it more convenient to use.}
@item{We used custom reference compilers to control the behavior of DSL-bound variables referenced in Racket expressions.}
@item{We used a rewrite rule and DSL macros to use struct names as the head of a DSL form.}
@item{We used a host interface to create a special case for the behavior of a DSL variable used in Racket expressions.}
@item{We used parameters to manage runtime state.}
@item{Our (recursive) pattern compiler has several invariants that inductively ensure its correctness.}
]

@;TODO actually follow this and make sure it runs
