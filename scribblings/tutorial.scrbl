#lang scribble/manual

@(require (for-label racket))

@title{Tutorial}

This guide demonstrates use of syntax-spec via the case study of constructing a DSL for structuring
code as state machines.

We will:

@itemlist[
 @item{Define the syntax (@secref["grammar"])}
 @item{Add binding rules (@secref["binding"])}
 @item{Integrate Racket subexpressions as guards (@secref["racket"])}
 @item{Compile to Racket code (@secref["compilation"])}
 @item{Allow macros to extend the language (@secref["macros"])}
 ]

Here's what using the DSL to define a controller for a subway turnstile looks like:

@(racketblock
  (define turnstile
    (machine
     #:initial locked

     (state locked
       (on (coin value) #:when (= value 0.25)
         (-> unlocked))
       (on (coin value)
         (-> locked)))
     
     (state unlocked
       (on (pass)
         (-> locked)))))

  (let-values ([(unlocked-turnstile _) (machine-step turnstile `(coin , 0.25))])
    (check-equal? (machine-state turnstile) '(locked))
    (check-equal? (machine-state unlocked-turnstile) '(unlocked))))

The machine has two states: locked and unlocked. It reacts to two kinds of external events: a coin
with a given value being inserted, and a person passing through the turnstile. Racket code uses
the machine via the @racket[machine-state] and @racket[machine-step] functions. Within the machine,
Racket code is used to implement the guard on the transition to the unlocked state, which checks
that the given coin is a quarter.

@section[#:tag "grammar"]{Grammar}

Our initial specification with the DSL supplies the grammar:

@(racketblock
  (syntax-spec
    (host-interface/expression
      (machine #:initial inital-state:id s:state-spec ...)

      (error 'machine "compiler not yet implemented"))
    
    (nonterminal state-spec
      (state n:id e:event-spec ...))

    (nonterminal event-spec
      (on (evt:id arg:id ...) t:transition-spec)
      (on (evt:id arg:id ...) #:when guard:guard-expr t:transition-spec))

    (nonterminal transition-spec
      (-> s:id))

    (nonterminal guard-expr
      v:id
      n:number
      (= e1:guard-expr e2:guard-expr))))

The @racket[host-interface/expression] declaration specifies that the @racket[machine] syntax extends
the language of Racket expressions. The first portion is a @emph{syntax specification}. It consists
of the form name (@racket[machine]), literal syntax like @racket[#:keyword], and named subexpressions
like @racket[s] together with references to the nonterminals that specify the syntax of the subexpression
like @racket[state-spec]. The remainder of the host interface declaration is compile-time Racket code.
Once the DSL syntax is checked and macro-expanded according to the syntax specification, this compile-time
code is responsible for compiling from the DSL to Racket. For now it's a stub.


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
@url{https://github.com/michaelballantyne/syntax-spec/blob/main/test/dsls/statecharts/statecharts.rkt}.

