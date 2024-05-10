#lang scribble/manual

@(require (for-label racket "../main.rkt"))

@title{Tutorial}

This guide demonstrates use of syntax-spec via the case study of constructing a DSL for structuring
code as state machines.

We will:

@itemlist[
 @item{Define the syntax (@secref["grammar"])}
 @item{Add binding rules (@secref["binding"])}
 @item{Integrate Racket subexpressions (@secref["racket"])}
 @item{Compile to Racket code (@secref["compilation"])}
 @item{Allow macros to extend the language (@secref["macros"])}
 ]

Here's what using the DSL to define a controller for a subway turnstile looks like:

@(racketblock
  (define turnstile%
    (machine
     #:initial locked

     (state locked
       (on (coin value) #:when (= value 0.25)
         (goto unlocked))
       (on (coin value)
         (goto locked)))
     
     (state unlocked
       (on (person-enters)
         (goto locked)))))

  (define ts (new turnstile%))
  (check-equal? (send ts get-state) 'locked)
  (send ts coin 0.25)
  (check-equal? (send ts get-state) 'unlocked))

The machine has two states: locked and unlocked. It reacts to two kinds of external events: a coin
with a given value being inserted, and a person passing through the turnstile.

The @racket[machine] declaration acts as a class. Racket code interacts with the machine
by constructing an instance and calling methods corresponding to machine transitions such as @racket[coin].
The @racket[get-state] method returns a symbol representing the current state.

Within the machine,
Racket code is used to implement the guard on the transition to the unlocked state, which checks
that the given coin is a quarter.



@section[#:tag "grammar"]{Grammar}

The essential parts of a DSL implementation in @racket[syntax-spec] are a specification of the DSL's syntax and a compiler that transforms DSL syntax to Racket.
@;
The syntax is specified in terms of @emph{nonterminals} with associated binding rules. We'll introduce binding rules later in the tutorial.
@;
@emph{Host interface macros} tie together the specification and the DSL compiler producing a Racket macro that forms the entry point to the language implementation.

Our initial specification with @racket[syntax-spec] supplies the grammar:

@codeblock|{
  #lang racket

  (require syntax-spec)
  
  (syntax-spec
    (host-interface/expression
      (machine #:initial inital-state:id s:state-spec ...)

      (error 'machine "compiler not yet implemented"))
    
    (nonterminal state-spec
      (state name:id transitions:transition-spec ...))

    (nonterminal transition-spec
      (on (event-name:id arg:id ...) action:action-spec)
      (on (event-name:id arg:id ...) #:when guard:guard-expr action:action-spec))

    (nonterminal action-spec
      (goto next-state-name:id))

    (nonterminal guard-expr
      var-ref:id
      n:number
      (= e1:guard-expr e2:guard-expr)))
}|

The @racket[syntax-spec] form is the entry-point into the metalanguage. It must be used at the top-level of a module.
@;
In the example above, the language definition contains two kinds of definitions, for host interface macros and nonterminals.

The @racket[host-interface/expression] form is used to define host interface macros that extend the language of Racket expressions.
@;
Here, it defines the @racket[machine] syntax for creating a state machine implemented as a Racket class.

The first part of the host interface definition specifies the syntax of the host interface macro, beginning with the name of the form:  @racket[machine].
@;
The remainder of the @racket[machine] form's syntax specification describes the literal elements of the syntax and its subexpression positions.
@;
Literal elements include keywords like @racket[#:initial].
@;
A colon-separated name like @racket[s:state-spec] indicates a subexpression position, where the first portion is the @emph{spec variable} used to name the position and the latter portion is a reference
to a nonterminal or binding class indicating the type of syntax that may appear in the subexpression.

The remainder of the host interface declaration is compile-time Racket code.
@;
Once the DSL syntax is checked and macro-expanded according to the syntax specification, this compile-time code is responsible for compiling from the DSL to Racket.
@;
For now it's a stub.


@section[#:tag "binding"]{Binding}

Consider this program:

@racketblock[
(machine
 #:initial red
 (state red
   (on (event x) #:when (= y 10)
     (goto green))
   (on (event x)
     (goto red))))
]

In the guard @racket[(= y 10)], the @racket[y] is unbound.
Additionally, our first transition is to @racket[green], but there is no @racket[green] state.
Our compiled code will end up containing an unbound variable reference for @racket[y], so Racket's
expander will raise an error.

However, let's say our compiler translates @racket[(goto green)] to @racket[(set! state 'green)] and doesn't
produce any identifiers for @racket[green],
and we changed the guard to @racket[(= x 10)] instead of @racket[(= y 10)], so there's no unbound reference to @racket[y].
Would we get an unbound reference error for @racket[green]? No! We'd just have strange behavior at runtime, or maybe
a runtime error, depending on the compiler.

We could adjust our compiler to check for unbound state references, but syntax-spec can do it for us. syntax-spec allows us to declare
the binding and scoping rules for our language, and bindings and references will be checked before your compiler is even
invoked, so your compiler can assume the program is not only grammatically correct, but also well-bound.

@subsection{Simple binding}

@; Problem: the binding we're showing here gets referenced in a Racket subexpression,
@; not in the DSL. So we can't talk about the full story without getting into compilation,
@; and we really need `host`.
@;
@; Solution possibility: include a small language of guard expressions in the DSL definition.
@; Then later show how we can integrate with Racket instead.

First, let's declare that the arguments to an action are in scope in the guard expression:

@(racketblock
  (syntax-spec
    (binding-class event-var)
    ...
    (nonterminal transition-spec
      (on (event-name:id arg:event-var ...) action:action-spec)
      #:binding (scope (bind arg))
      (on (event-name:id arg:event-var ...) #:when guard:guard-expr action:action-spec)
      #:binding (scope (bind arg) guard))
    ...
    (nonterminal guard-expr
      var-ref:event-var
      n:number
      (= e1:guard-expr e2:guard-expr))))

We added a binding class, @racket[event-var], for an event's argument names. We also added a @racket[#:binding] declaration to guarded transitions to declare that the @racket[arg]s are bound in the @racket[guard] expression and this binding introduces a new scope. Although there are no reference positions in a non-guarded transition, we still need to declare the binding rule. Otherwise, by default, syntax-spec will assume that the @racket[arg] is a reference position, which will cause @racket[arg] to be unbound. When we don't include any binding rule for a production at all, a default binding rule is implicitly generated which treats all forms as reference positions.

These simple binding rules behave like @racket[let]:

@racketblock[
(syntax-spec
  (binding-class my-var)
  (nonterminal my-expr
    (my-let ([x:my-var e:my-expr] ...) body:my-expr)
    #:binding (group e (scope (bind x) body))
    x:my-var
    n:number))
]

We could've just written @racket[(scope (bind x) body)]. syntax-spec will automatically treat @racket[e] as a reference position outside of the new scope. That's why we don't have to mention @racket[action] or @racket[event-name] in the binding rules for transitions.

@subsection{Definition contexts}

Now let's add binding rules for state names. We can't just use @racket[scope] and @racket[bind] since the binding for state names is not like @racket[let]. It's more like @racket[define] where you can have mutual recursion. For that kind of binding structure, we use @racket[export] and @racket[import]:

@(racketblock
  (binding-class state-name))

@(racketblock
  (host-interface/expression
    (machine #:initial inital-state:state-name s:state-spec ...)
    #:binding (scope (import s) initial-state)
    (error 'machine "compiler not yet implemented"))

  (nonterminal/exporting state-spec
    (state name:state-name transition:transition-spec ...)
    #:binding (export name)))

We use an exporting nonterminal for @racket[state-spec], which allows us to use the @racket[export] binding rule for mutually recursive definitions. This binds @racket[name] in @racket[transition] and the other @racket[state-spec] forms in the body of the machine, like @racket[define] in a @racket[class] body.

Similar to @racket[bind] for a variable, we use @racket[import] to declare that an exporting nonterminal's bindings should be in scope for the @racket[initial-state] in the @racket[machine].

@subsection{Nested binding}

@(racketblock
  (let* ([b:binding-pair ...]) e:guard-expr))

@(racketblock
  (nonterminal binding-pair
    [v:local-var e:guard-expr]))

@(racketblock
  (let* ([b:binding-pair ...]) e:guard-expr)
  #:binding (nest b e))

@(racketblock
  (nesting-nonterminal binding-pair (nested)
    [v:local-var e:guard-expr]
    #:binding (scope (bind v) nested)))

@section[#:tag "racket"]{Integrating Racket Subexpressions}

@section[#:tag "compilation"]{Compilation}

@section[#:tag "macros"]{Macros}

The full code for the state machine example is available at
@url{https://github.com/michaelballantyne/syntax-spec/blob/main/tests/dsls/state-machine-oo}.

@;TODO demonstrate symbol tables by doing an arity check on actions?
