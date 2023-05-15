#lang scribble/manual

@(require (for-label racket syntax-spec))

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
         (-> unlocked))
       (on (coin value)
         (-> locked)))
     
     (state unlocked
       (on (person-enters)
         (-> locked)))))

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

@subsection{Simple binding}

@; Problem: the binding we're showing here gets referenced in a Racket subexpression,
@; not in the DSL. So we can't talk about the full story without getting into compilation,
@; and we really need `host`.
@;
@; Solution possibility: include a small language of guard expressions in the DSL definition.
@; Then later show how we can integrate with Racket instead.
@(racketblock
  (binding-class local-var))

@(racketblock
  (nonterminal event-spec
    (on (evt:id) t:transition-spec)
    (on (evt:id arg:local-var ...) #:when guard:guard-expr t:transition-spec)
    #:binding {(bind arg) guard}))


@subsection{Definition contexts}

@(racketblock
  (binding-class state-name))

@(racketblock
  (machine #:initial inital-state:state-name s:state-spec ...)
  #:binding {(recursive s) initial-state})

@(racketblock
  (nonterminal/two-pass state-spec
    (state n:state-name e:event-spec ...)
    #:binding (export n)))

@(racketblock
  (nonterminal transition-spec
    (-> s:state-name)))


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
    #:binding {(bind v) nested}))

@section[#:tag "racket"]{Integrating Racket Subexpressions}

@section[#:tag "compilation"]{Compilation}

@section[#:tag "macros"]{Macros}

The full code for the state machine example is available at
@url{https://github.com/michaelballantyne/syntax-spec/blob/main/tests/dsls/state-machine-oo}.

