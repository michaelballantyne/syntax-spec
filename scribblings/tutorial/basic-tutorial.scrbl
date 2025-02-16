#lang scribble/manual

@(require (for-label racket racket/block racket/class racket/match racket/list syntax/parse "../../main.rkt")
          scribble/example)

@title{Basic Tutorial: State Machine Language}

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

Here's what using the DSL to define a controller for a vending machine looks like:

@(racketblock
(define vending-machine
  (machine
   #:initial idle
   (state idle
     (on-enter (displayln "pay a dollar"))
     (on (dollar)
       (goto paid))
     (on (select-item item)
       (displayln "you need to pay before selecting an item")
       (goto idle)))
   (state paid
     (on-enter (displayln "select an item"))
     (on (select-item item)
       (displayln (format "dispensing ~a" item))
       (goto idle)))))
)

The vending machine has two states: idle and paid. It reacts to two kinds of external events: a dollar
being inserted, and an item being selected for purchase.

The @racket[machine] declaration acts as a class. Racket code interacts with the machine
by constructing an instance and calling methods corresponding to machine transitions such as @racket[dollar].
The @racket[get-state] method returns a symbol representing the current state.

Within the machine, Racket code can be run when certain states are entered or certain transitions occur. Within transitions, these actions can reference arguments to the transition event, such as @racket[item] in the @racket[select-item] event.

@section[#:tag "grammar"]{Grammar}

The essential parts of a DSL implementation in @racket[syntax-spec] are a specification of the DSL's syntax and a compiler that transforms DSL syntax to Racket.
@;
The syntax is specified in terms of @emph{nonterminals} with associated binding rules. We'll introduce binding rules later in the tutorial.
@;
@emph{Host interface macros} tie together the specification and the DSL compiler producing a Racket macro that forms the entry point to the language implementation.

Our initial specification with @racket[syntax-spec] supplies the grammar:

@codeblock|{
  #lang racket

  (require syntax-spec-dev (for-syntax syntax/parse racket/list))

  (syntax-spec
    (host-interface/expression
      (machine #:initial initial-state:id s:state-spec ...)

      (error 'machine "compiler not yet implemented"))
    
    (nonterminal state-spec
      (state name:id transitions:transition-spec ...)
      (state name:id ((~datum on-enter) body:action-spec ...+) transitions:transition-spec ...))

    (nonterminal transition-spec
      (on (event-name:id arg:id ...)
        action:action-spec
        ...
        ((~datum goto) next-state:id)))

    (nonterminal action-spec
      ((~datum displayln) x:id)))
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
   (on (event x)
     (goto green))
   (on (event x)
     (goto red))))
]

Our first transition is to @racket[green], but there is no @racket[green] state. This should result in an unbound variable error.

However, let's say our compiler translates @racket[(goto green)] to @racket[(set! state 'green)] and doesn't
produce any identifiers for @racket[green].
Would we get an unbound reference error for @racket[green]? No! We'd just have strange behavior at runtime, or maybe
a runtime error, depending on the compiler.

We could adjust our compiler to check for unbound state references, but syntax-spec can do it for us. syntax-spec allows us to declare
the binding and scoping rules for our language, and bindings and references will be checked before your compiler is even
invoked, so your compiler can assume the program is not only grammatically correct, but also well-bound.

There are also several other benefits that we get by providing binding rules. We can use symbol tables to associate information with identifiers, we can allow our languages to have hygienic macros, we can compute the free identifiers of an expression, and many other identifier-related operations. We'll get more into these details later, but the point is you get a lot for free by declaring binding rules. This is why you should be excited!

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
      (on (event-name:id arg:event-var ...)
        action:action-spec
        ...
        ((~datum goto) next-state:id))
      #:binding (scope (bind arg) ... action ...))


    (nonterminal action-spec
      ((~datum displayln) x:event-var))))

We added a binding class, @racket[event-var], for an event's argument names. We also added a @racket[#:binding] declaration to transition actions to declare that the @racket[arg]s are bound in the @racket[action] expressions and this binding introduces a new scope.

These simple binding rules behave like @racket[let]:

@racketblock[
(syntax-spec
  (binding-class my-var)
  (nonterminal my-expr
    (my-let ([x:my-var e:my-expr] ...) body:my-expr)
    #:binding [e ... (scope (bind x) ... body)]
    x:my-var
    n:number))
]

We could've just written @racket[(scope (bind x) ... body)]. syntax-spec will automatically treat @racket[e] as a reference position outside of the new scope. That's why we don't have to mention @racket[event-name] in the binding rules for transitions. Additionally, for @racket[action-spec] expressions, there is an implicit @racket[#:binding] rule generated that treats @racket[x] as a reference position.

Notice that there are ellipses in the binding spec corresponding to the ellipses in the syntax spec. Like with syntax patterns and syntax templates, ellipses allow us to control the binding structure of syntax with sequences like @racket[[x:my-var e:my-expr] ...].

All spec references in a binding spec must have the same depth as their syntax spec counterparts. This is stricter than syntax templates, where it is possible for a template variable to occur with greater ellipsis depth than its associated pattern variable.

@subsection{Separate scope and binding forms}

Now let's add binding rules for state names. We can't just use @racket[scope] and @racket[bind] since the binding of the state name comes from the @racket[state-spec] nonterminal, and those bindings need to be in scope throughout the entire @racket[machine] form. To use @racket[bind], we need to be able to refer to the name being bound directly. For this kind of binding structure, we use @racket[export] to export bindings from the @racket[state-spec] nonterminal and @racket[import] to import those bindings into a scope in the @racket[machine] host interface:

@(racketblock
  (binding-class state-name))

@(racketblock
  (host-interface/expression
    (machine #:initial initial-state:state-name s:state-spec ...)
    #:binding (scope (import s) ... initial-state)
    (error 'machine "compiler not yet implemented"))

  (nonterminal/exporting state-spec
    (state name:state-name ((~datum on-enter) body:action-spec ...+)  transition:transition-spec ...)
    #:binding (export name)

    (state name:state-name transition:transition-spec ...)
    #:binding (export name))

  (nonterminal transition-spec
    (on (event-name:id arg:event-var ...)
      action:action-spec
      ...
      ((~datum goto) next-state:state-name))
    #:binding (scope (bind arg) ... action ...)))

We use an exporting nonterminal for @racket[state-spec], which allows us to use the @racket[export] binding rule. This binds @racket[name] in @racket[transition] and the other @racket[state-spec] forms in the body of the machine, like @racket[define] in a @racket[class] body or a @racket[block] form.

Similar to @racket[bind] for a variable, we use @racket[import] to declare that an exporting nonterminal's bindings should be in scope for the @racket[initial-state] in the @racket[machine].

@subsection{Nested binding}

There is another type of binding rule that doesn't fit into our state machine language, but you might need it when creating a different language. This is nested binding and behaves like @racket[let*], where you have a sequence of variables being defined and each one is in scope for the subsequent definitions (but not previous ones). Here is an example:

@(racketblock
  (syntax-spec
    (binding-class my-var)
    (nonterminal my-expr
      (my-let* (b:binding-pair ...) body:my-expr)
      #:binding (nest b ... body)
      n:number
      x:my-var)
    (nonterminal/nesting binding-pair (nested)
      [x:my-var e:my-expr]
      #:binding (scope (bind x) nested))))

We create a nesting nonterminal for a binding pair, which has @racket[nested], which is like an argument for the nonterminal's binding rules. This represents the @tech{scope tree} of the rest of the binding rules. In this case, the scope tree gets built up sort of like @racket[foldr] on a list.

The @deftech{scope tree} is a first-class representation of the binding structure of the program. It's not something that you explicitly work with, but it's useful to know about. Conceptually, syntax-spec uses your language's binding rules to construct this scope tree during expansion.

From the simple nonterminal @racket[my-expr], we put the @racket[binding-pair]'s bindings in scope using @racket[nest], providing @racket[body] as the intial value of @racket[nested], like the base case value of @racket[foldr].

Since we're folding over the sequence of @racket[b]s, the ellipses are inside of the @racket[nest].

@section[#:tag "racket"]{Integrating Racket Subexpressions}

In our state machine language, action expressions are very limited. Let's remind ourselves what the grammar for an action expression looks like:

@(racketblock
   (nonterminal action-spec
      ((~datum displayln) x:event-var)))

An action expression can only @racket[displayln] the value of a variable. What if we want something fancier, like using @racket[format] inside the @racket[displayln]? Really, it'd be ideal to be able to allow arbitrary racket expressions for the action. We can actually do that!

@(racketblock
   (syntax-spec
     ...

     (nonterminal/exporting state-spec
       (state name:state-name ((~datum on-enter) body:racket-body ...+)  transition:transition-spec ...)
       #:binding [(export name) (scope (import body) ...)]

       (state name:state-name transition:transition-spec ...)
       #:binding (export name))

     (nonterminal transition-spec
       (on (event-name:id arg:event-var ...)
         body:racket-body
         ...
         ((~datum goto) next-state-name:state-name))
       #:binding (scope (bind arg) ... (import body) ...))

     ...))

Instead of using @racket[action-spec] and defining our own nonterminal for action expressions, we can just use @racket[racket-body], which allows arbitrary racket expressions and definitions. And our @racket[event-var] identifiers will be in scope in the racket expression! We can control how references to our DSL-bound variables behave in Racket expressions and whether they're allowed at all using reference compilers, which we'll discuss in the @secref["compilation"] section.

In addition to @racket[racket-body], syntax-spec provides @racket[racket-expr] for allowing Racket expressions, @racket[racket-var] for allowing references to Racket-defined variables in DSL expressions, and @racket[racket-macro] for allowing the language to be extended by arbitrary Racket macros. We'll talk more about macros in the @secref["macros"] section.

@section[#:tag "compilation"]{Compilation}

Now that we have our grammar and binding rules defined, we must write a compiler to translate a state machine program to Racket. We already have a host interface macro defined, which is the entry point to our DSL:

@racketblock[
(syntax-spec
  ...
  (host-interface/expression
    (machine #:initial initial-state:state-name s:state-spec ...)
    #:binding (scope (import s) ... initial-state)
    (error 'machine "compiler not yet implemented"))
  ...)
]

However, our compiler, which performs the actual translation, is not defined. The compiler is a macro that translates our state machine language to Racket code. In our compiler, we'll translate the state machine to Racket classes using @hyperlink["https://en.wikipedia.org/wiki/State_pattern"]{the state machine pattern}.

For example, let's imagine how we'd translate the example state machine:

@(racketblock
(define vending-machine
  (machine
   #:initial idle
   (state idle
     (on-enter (displayln "pay a dollar"))
     (on (dollar)
       (goto paid))
     (on (select-item item)
       (displayln "you need to pay before selecting an item")
       (goto idle)))
   (state paid
     (on-enter (displayln "select an item"))
     (on (select-item item)
       (displayln (format "dispensing ~a" item))
       (goto idle)))))
)

We'll create a class for the state machine, which acts as a context class, and a class for each state:

@(racketblock
(let ()
  (define machine%
    (class object%
      (define state #f)
      (define/public (set-state state%)
        (set! state (new state% [machine this])))
      (define/public (get-state)
        (send state get-state))

      (define/public (dollar)
        (send/apply state dollar))

      (define/public (select-item item)
        (send/apply state select-item item))

      (send this set-state idle)
      (super-new)))

  (define idle
    (class object%
      (init-field machine)
      (define/public (get-state)
        'idle)

      (displayln "pay a dollar")

      (define/public (dollar)
        (send machine set-state paid))

      (define/public (select-item item)
        (displayln "you need to pay before selecting an item")
        (send machine set-state idle))
      (super-new)))

  (define paid
    (class object%
      (init-field machine)
      (define/public (get-state)
        'idle)

      (displayln "select an item")

      (define/public (select-item item)
        (displayln (format "dispensing ~a" item))
        (send machine set-state idle))
      (super-new)))

  (new machine%))
)

The @racket[machine%] class stores the current state instance and delegates to it. Each state class has methods for each defined transition. Transition actions go in the transition's method and @racket[on-enter] actions go in the class body. When a state is entered, the @racket[machine%] class creates a fresh instance of it, which runs the class body, and sets the current state to that instance. Finally, we return an instance of the machine class.

Now Let's start to write the compiler:

@racketblock[
(syntax-spec
  (binding-class event-var #:reference-compiler mutable-reference-compiler)
  ...
  (host-interface/expression
    (machine #:initial initial-state:state-name s:state-spec ...)
    #:binding (scope (import s) ... initial-state)
    #'(compile-machine initial-state s ...))
  ...)

(define-syntax compile-machine
  (syntax-parser
    #:datum-literals (machine state on-enter)
    [(_ initial-state
        (state state-name
          (~optional (on-enter action ...) #:defaults ([(action 1) '()]))
          e ...)
        ...)
     #'(let ()
         (define machine%
           (class object%
             (define state #f)
             (define/public (set-state state%)
               (set! state (new state% [machine this])))
             (define/public (get-state)
               (send state get-state))

             (compile-proxy-methods (e ... ...) state)

             (send this set-state initial-state)
             (super-new)))

         (define state-name
           (class object%
             (init-field machine)
             (define/public (get-state)
               'state-name)
             action ...
             (compile-event-method e machine) ...
             (super-new)))
         ...

         (new machine%))]))
]

We defined a macro, @racket[compile-machine], which expands to something similar to what we wrote by hand above. One thing we have to do with syntax-spec is declare a reference compiler in the @racket[binding-class] declaration. This allows us to control whether and how DSL identifiers behave in Racket expressions like actions. In our case, we use @racket[mutable-reference-compiler], which allows event arguments to be referenced and mutated. We don't specify a reference compiler for state names, so they cannot be referenced in Racket expressions. Only @racket[goto].

We have helpers to define the proxy methods in the @racket[machine%] class and transition methods in the state classes:

@(racketblock
(define-syntax compile-proxy-methods
  (syntax-parser
    #:datum-literals (on goto)
    [(_ ((on (event-name . _) . _) ...) target)
     #:with (unique-event ...)
     (remove-duplicates (map syntax-e (attribute event-name)))
     #'(begin
         (define/public (unique-event . args)
           (send/apply target unique-event args))
         ...)]))

(define-syntax compile-event-method
  (syntax-parser
    #:datum-literals (on goto)
    [(_ (on (event-name arg ...)
          action ...
          (goto name))
        machine)
     #'(define/public (event-name arg ...)
         action ...
         (send machine set-state name))]))
)

For @racket[compile-proxy-methods], to generate one method definition for each possible transition, we gather up all the transitions in @racket[compile-machine] with that @racket[(e ... ...)], remove the duplicate transition event names, and define a proxy method for each one that delegates to the state instance, which is passed in as @racket[target]. #racket[compile-event-method] is pretty straightforward.

One thing to note is that Racket expressions like @racket[action] in @racket[compile-event-method] get wrapped in a @racket[#%host-expression] form by syntax-spec. You can usually ignore this fact completely when writing a compiler, but if you try to inspect the contents of a Racket expression in a compiler, you'll have to account for it.

Now we have all the pieces to run programs using state machines:

@examples[
#:label #f
(require racket/class
         syntax-spec-dev/tests/dsls/state-machine-for-tutorial)
(define vending-machine
  (machine
   #:initial idle
   (state idle
     (on-enter (displayln "pay a dollar"))
     (on (dollar)
       (goto paid))
     (on (select-item _)
       (displayln "you need to pay before selecting an item")
       (goto idle)))
   (state paid
     (on-enter (displayln "select an item"))
     (on (select-item item)
       (displayln (format "dispensing ~a" item))
       (goto idle)))))
(send vending-machine get-state)
(send vending-machine select-item "chips")
(send vending-machine get-state)
(send vending-machine dollar)
(send vending-machine get-state)
(send vending-machine select-item "chips")
(send vending-machine get-state)
]

@subsection[#:tag "symbol tables"]{Symbol Tables}

@;TODO are there significant benefits to symbol tables over free/bound id tables?

Symbol tables and symbol sets allow us to associate information with identifiers, similar to @secref["idtable" #:doc '(lib "syntax/scribblings/syntax.scrbl")] and @secref["idset" #:doc '(lib "syntax/scribblings/syntax.scrbl")], but for DSL identifiers.

In our language's compiler, we can use symbol set to raise an error when a state is unreachable:

@racketblock[
(syntax-spec
  ...
  (host-interface/expression
    (machine #:initial initial-state:state-name s:state-spec ...)
    #:binding (scope (import s) ... initial-state)
    (check-for-inaccessible-states #'initial-state (attribute s))
    #'(compile-machine initial-state s ...))
  ...)

(begin-for-syntax
  (define (check-for-inaccessible-states initial-state-id state-specs)
    (define accessible-states (get-accessible-states initial-state-id state-specs))
    (for/list ([state-spec state-specs]
               #:unless (symbol-set-member? accessible-states (state-spec-name state-spec)))
      (error 'machine "Inaccessible state: ~a" (syntax->datum (state-spec-name state-spec)))))

  (define (get-accessible-states initial-state-id state-specs)
    (define accessible-states (local-symbol-set))
    (define (find-state-spec state-name)
      (findf (lambda (state-spec)
               (compiled-identifier=? state-name (state-spec-name state-spec)))
             state-specs))
    (define (add-reachable-states! state-name)
      (unless (symbol-set-member? accessible-states state-name)
        (symbol-set-add! accessible-states state-name)
        (define state-spec (find-state-spec state-name))
        (for ([next-state-name (state-spec-next-state-names state-spec)])
          (add-reachable-states! next-state-name))))
    (add-reachable-states! initial-state-id)
    accessible-states)

  (define (state-spec-name state-spec)
    (syntax-parse state-spec
      [(state name . _) #'name]))

  (define (state-spec-next-state-names state-spec)
    (syntax-parse state-spec
      [(state name
         (~or ((~datum on-enter) . _)
              ((~datum on) ev
                  body
                  ...
                  (goto next-state-name)))
         ...)
       (attribute next-state-name)])))
]

We build up a symbol set of accessible states with a depth-first search over the possible transitions starting from the initial state, and if we find a state that isn't accessible, we error.

This static check runs before we generate the compiled code. Compilers may have many static analysis passes like this one, or even passes that emit an intermediate representation like ANF. There are some special considerations to be made when creating multi-pass compilers with intermediate representations in syntax-spec which are covered in @secref["multipass"]

@examples[
#:label #f
(require racket/class
         syntax-spec-dev/tests/dsls/state-machine-for-tutorial)
(eval:error
 (define gas-tank
   (machine
     #:initial full

     (state empty
       (on (re-fuel)
         (goto full)))

     (state full))))
]

We forgot to add a transition to go from full to empty. And since we start on full, there is no way to get to empty.

@section[#:tag "macros"]{Macros}

syntax-spec allows us to make our DSLs macro-extensible. For example, let's allow users to create macros for definining states:

@racketblock[
(syntax-spec
  ...
  (extension-class state-macro)

  (nonterminal/exporting state-spec
    #:allow-extension state-macro

    ...))

(define-syntax-rule
  (define-state-syntax name trans)
  (define-dsl-syntax name state-macro trans))
]

By adding an extension class called @racket[state-macro] and allowing @racket[state-spec] to be extended by these state macros, transformers wrapped with @racket[state-macro] can be used in @racket[state-spec] positions. syntax-spec provides @racket[define-dsl-syntax] for defining these wrapped transformers. These macros will be hygienic in our DSL. Since only certain nonterminals are extensible by certain extension classes, we can control what kinds of macros can be used where.

Now let's create a macro in our language!

@examples[#:label #f
(require racket/class
         syntax-spec-dev/tests/dsls/state-machine-for-tutorial)
(define-state-syntax simple-state
    (syntax-rules ()
      [(_ name [evt next] ...)
       (state name
              (on (evt) (goto next))
              ...)]))
(define traffic-light
  (machine
   #:initial red
   (simple-state red [tick green])
   (simple-state green [tick yellow])
   (simple-state yellow [tick red])))
(send traffic-light get-state)
(send traffic-light tick)
(send traffic-light get-state)
(send traffic-light tick)
(send traffic-light get-state)
(send traffic-light tick)
(send traffic-light get-state)
]

The full code for the state machine example is available at
@url{https://github.com/michaelballantyne/syntax-spec/blob/main/tests/dsls/state-machine-for-tutorial.rkt}.

There is also an example of using the state machine language to create a CSV browser with a GUI at @url{https://github.com/michaelballantyne/syntax-spec/blob/main/demos/minimal-state-machine/csv-browser.rkt}
