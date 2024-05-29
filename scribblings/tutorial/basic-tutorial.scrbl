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

Here's what using the DSL to define a controller for a subway turnstile looks like:

@(racketblock
  (define turnstile
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
      (machine #:initial initial-state:id s:state-spec ...)

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

There are also several other benefits that we get by providing binding rules. We can use symbol tables to associate information with identifiers, it allows our languages to have hygienic macros, we can compute the free identifiers of an expression, and many other identifier-related operations. We'll get more into these details later, but the point is you get a lot for free by declaring binding rules. This is why you should be excited!

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
    #:binding [e (scope (bind x) body)]
    x:my-var
    n:number))
]

We could've just written @racket[(scope (bind x) body)]. syntax-spec will automatically treat @racket[e] as a reference position outside of the new scope. That's why we don't have to mention @racket[action] or @racket[event-name] in the binding rules for transitions.

@subsection{Separate scope and binding forms}

Now let's add binding rules for state names. We can't just use @racket[scope] and @racket[bind] since the binding of the state name comes from the @racket[state-spec] nonterminal, and those bindings need to be in scope throughout the entire @racket[machine] form. To use @racket[bind], we need to be able to refer to the name being bound directly. For this kind of binding structure, we use @racket[export] to export bindings from the @racket[state-spec] nonterminal and @racket[import] to import those bindings into a scope in the @racket[machine] host interface:

@(racketblock
  (binding-class state-name))

@(racketblock
  (host-interface/expression
    (machine #:initial initial-state:state-name s:state-spec ...)
    #:binding (scope (import s) initial-state)
    (error 'machine "compiler not yet implemented"))

  (nonterminal/exporting state-spec
    (state name:state-name transition:transition-spec ...)
    #:binding (export name))

  (nonterminal action-spec
    (goto next-state-name:state-name)))

We use an exporting nonterminal for @racket[state-spec], which allows us to use the @racket[export] binding rule. This binds @racket[name] in @racket[transition] and the other @racket[state-spec] forms in the body of the machine, like @racket[define] in a @racket[class] body or a @racket[block] form.

Similar to @racket[bind] for a variable, we use @racket[import] to declare that an exporting nonterminal's bindings should be in scope for the @racket[initial-state] in the @racket[machine].

@subsection{Nested binding}

@;TODO move this to the advanced tutorial and leave a note here

There is another type of binding rule that doesn't fit into our state machine language, but you might need it when creating a different language. This is nested binding and behaves like @racket[let*], where you have a sequence of variables being defined and each one is in scope for the subsequent definitions (but not previous ones). Here is an example:

@(racketblock
  (syntax-spec
    (binding-class my-var)
    (nonterminal my-expr
      (my-let* (b:binding-pair ...) body:my-expr)
      #:binding (nest b body)
      n:number
      x:my-var)
    (nonterminal/nesting binding-pair (nested)
      [x:my-var e:my-expr]
      #:binding (scope (bind x) nested))))

We create a nesting nonterminal for a binding pair, which has @racket[nested], which is like an argument for the nonterminal's binding rules. This represents the @tech{scope tree} of the rest of the binding rules. In this case, the scope tree gets built up sort of like @racket[foldr] on a list.

The @deftech{scope tree} is a first-class representation of the binding structure of the program. It's not something that you explicitly work with, but it's useful to know about. Conceptually, syntax-spec uses your language's binding rules to construct this scope tree during expansion.

From the simple nonterminal @racket[my-expr], we put the @racket[binding-pair]'s bindings in scope using @racket[nest], providing @racket[body] as the intial value of @racket[nested], like the base case value of @racket[foldr].

@section[#:tag "racket"]{Integrating Racket Subexpressions}

In our state machine language, guard expressions are very limited. Let's remind ourselves what the grammar for a guard expression looks like:

@(racketblock
   (nonterminal guard-expr
     var-ref:event-var
     n:number
     (= e1:guard-expr e2:guard-expr)))

A guard expression is either a variable reference, a number, or an equality test. What if we want something fancier like @racket[<]? Or what if we want to use values other than numbers? Really, it'd be ideal to be able to allow arbitrary racket expressions for the guard. We can actually do that!

@(racketblock
   (syntax-spec
     ...
     (nonterminal transition-spec
       (on (event-name:id arg:event-var ...) action:action-spec)
       #:binding (scope (bind arg))
       (on (event-name:id arg:event-var ...) #:when guard:racket-expr action:action-spec)
       #:binding (scope (bind arg) guard))
     ...))

Instead of using @racket[guard-expr] and defining our own nonterminal for guard expressions, we can just use @racket[racket-expr], which allows arbitrary racket expressions. And our @racket[event-var] identifiers will be in scope in the racket expression! We can control how references to our DSL-bound variables behave in Racket expressions and whether they're allowed at all using reference compilers, which we'll discuss in the @secref["compilation"] section.

In addition to @racket[racket-expr], syntax-spec provides @racket[racket-var] for allowing references to Racket-defined variables in DSL expressions, and @racket[racket-macro] for allowing the language to be extended by arbitrary Racket macros. We'll talk more about macros in the @secref["macros"] section.

@section[#:tag "compilation"]{Compilation}

Now that we have our grammar and binding rules defined, we must write a compiler to translate a state machine program to Racket. We already have a host interface macro defined, which is the entry point to our DSL:

@racketblock[
(syntax-spec
  ...
  (host-interface/expression
    (machine #:initial initial-state:state-name s:state-spec ...)
    #:binding (scope (import s) initial-state)
    (error 'machine "compiler not yet implemented"))
  ...)
]

However, our compiler, which performs the actual translation, is not defined. Let's start writing it:

@racketblock[
(syntax-spec
  ...
  (host-interface/expression
    (machine #:initial initial-state:state-name s:state-spec ...)
    #:binding (scope (import s) initial-state)
    #'(compile-machine initial-state s^ ...))
  ...)

(define-syntax compile-machine
  (syntax-parser
    [(_ initial-state:id
        (~and state-spec
              ((~literal state) state-name evt ...))
        ...)
     (define/syntax-parse (event-name ...) (unique-event-names #'(evt ... ...)))
     (define/syntax-parse (event-method ...)
       (for/list ([event-name (attribute event-name)])
         (compile-event-method event-name (attribute state-spec))))
     #'(with-reference-compilers ([event-var immutable-reference-compiler])
         ; no reference compiler for state names since they shouldn't be referenced in racket expressions.
         (let ()
           (define machine%
             (class object%
               (super-new)
               (define state 'initial-state)
               (define/public (set-state! new-state)
                 (set! state new-state))
               (define/public (get-state) state)
               event-method
               ...))

           (new machine%)))]))]

We defined a macro, @racket[compile-machine], which emits a class definition for the machine and ultimately evaluates to an instance of the machine class. Most of this has nothing to do with syntax-spec and is what you'd see in a typical Racket macro compiler for a DSL. However, notice the use of @racket[with-reference-compilers]. This is from syntax-spec and it allows us to control how DSL-bound variables behave in Racket expression positions like the guard of a transition spec. In this case, we chose to use @racket[immutable-reference-compiler] to prevent mutation of @racket[event-var] variables. We intentionally don't provide a reference compiler for @racket[state-name] identifiers because they shouldn't be accessible from Racket expressions, only our DSL's @racket[goto] form.

Now let's define the helpers referenced here:

@racketblock[#:escape unracket
(begin-for-syntax
  ; Syntax -> Identifier
  (define (state-spec-name state-spec)
    (syntax-parse state-spec
      [(state name . _) #'name]))

  (define (unique-event-names evt-stxs)
    (remove-duplicates (map event-name (syntax->list evt-stxs))
                       (lambda (a b) (eq? (syntax->datum a) (syntax->datum b)))))

  (define (event-name e)
    (syntax-parse e
      [(on (name . _) . _) #'name])))

(begin-for-syntax
  ; Identifier (listof Syntax) -> Syntax
  (define (compile-event-method event-name state-specs)
    (define/syntax-parse (state-name ...)
      (for/list ([state-spec state-specs])
        (state-spec-name state-spec)))
    (define/syntax-parse (state-spec ...) state-specs)
    #`(define/public (#,event-name . args)
        (match (send this get-state)
          ['state-name
           (apply (compile-event-handler-for-state #,event-name state-name (state-spec ...))
                  args)]
          ...
          [state (error 'machine (format "Unknown state: ~a" state))]))))

(define-syntax compile-event-handler-for-state
  (syntax-parser
    [(_ event-name state-name (state-spec ...))
     (define/syntax-parse
       ((on (_ arg ...) (~optional (~seq #:when guard) #:defaults ([guard #'#t]))
            (goto next-state-name))
        ...)
       (get-transitions-for-event-and-state #'event-name #'state-name #'(state-spec ...)))
     #'(lambda args
         (cond
           [(apply (lambda (arg ...) guard)
                   args)
            (send this set-state! 'next-state-name)]
           ...
           [else (error 'machine
                        "No transition defined for event ~v in state ~v"
                        (syntax->datum #'event-name)
                        (syntax->datum #'state-name))]))]))

(begin-for-syntax
  ; Identifier Identifier (listof Syntax) -> (listof Syntax)
  ; gets the transitions for the given event and state
  (define (get-transitions-for-event-and-state evt-name state-name state-specs)
    (apply append
           (for/list ([state-spec (syntax->list state-specs)]
                      #:when (compiled-identifier=? (state-spec-name state-spec)
                                                    state-name))
             (syntax-parse state-spec
               [(state _
                       transition
                       ...)
                (for/list ([transition (attribute transition)]
                           #:when (eq? (syntax->datum evt-name)
                                       (syntax->datum (event-name transition))))
                  transition)])))))]

Most of these helpers don't involve anything syntax-spec specific, so we won't talk about them much. For each event, we  One thing to note is that Racket expressions like @racket[guard] in @racket[compile-event-handler-for-state] get wrapped in a @racket[#%host-expression] form by syntax-spec. You can usually ignore this fact completely when writing a compiler, but if you try to inspect the contents of a Racket expression in a compiler, you'll have to account for it.

Our last helper, @racket[get-transitions-for-event-and-state], uses @racket[compiled-identifier=?] from syntax-spec to compare state names. syntax-spec compiles and renames DSL identifiers to ensure proper hygiene and allow for some utilities like symbol tables, which we'll discuss soon. We compare DSL identifiers using @racket[compiled-identifier=?].

Now we have all the pieces to run programs using state machines:

@examples[
#:label #f
(require racket/class
         syntax-spec/tests/dsls/state-machine-for-tutorial)
(define turnstile
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
(send turnstile get-state)
(send turnstile coin 0.05)
(send turnstile get-state)
(send turnstile coin 0.25)
(send turnstile get-state)
(send turnstile person-enters)
(send turnstile get-state)
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
    #:binding (scope (import s) initial-state)
    (check-for-inaccessible-states #'initial-state (attribute s))
    #'(compile-machine initial-state s^ ...))
  ...)

(begin-for-syntax
  (define (check-for-inaccessible-states initial-state-id state-specs)
    (define accessible-states (get-accessible-states initial-state-id state-specs))
    (for/list ([state-spec state-specs]
               #:unless (symbol-set-member? accessible-states (state-spec-name state-spec)))
      (error 'machine "Inaccessible state: ~a" (syntax->datum (state-spec-name state-spec)))))

  (define (get-accessible-states initial-state-id state-specs)
    (define-local-symbol-set accessible-states)
    (define (find-state-spec state-name)
      (findf (lambda (state-spec)
               (compiled-identifier=? state-name (state-spec-name state-spec)))
             state-specs))
    (let loop ([state-name initial-state-id])
      (unless (symbol-set-member? accessible-states state-name)
        (symbol-set-add! accessible-states state-name)
        (define state-spec (find-state-spec state-name))
        (for ([next-state-name (state-spec-next-state-names state-spec)])
          (loop next-state-name))))
    accessible-states)

  (define (state-spec-name state-spec)
    (syntax-parse state-spec
      [(state name . _) #'name]))

  (define (state-spec-next-state-names state-spec)
    (syntax-parse state-spec
      [(state name
         (on action (~optional (~seq #:when guard))
             (goto next-state-name))
         ...)
       (attribute next-state-name)])))
]

We build up a symbol set of accessible states with a depth-first search over the possible transitions starting from the initial state, and if we find a state that isn't accessible, we error.

This static check runs before we generate the compiled code. Compilers may have many static analysis passes like this one, or even passes that emit an intermediate representation like ANF. There are some special considerations to be made when creating multi-pass compilers with intermediate representations in syntax-spec which are covered in @secref["multipass example"]

@examples[
#:label #f
(require racket/class
         syntax-spec/tests/dsls/state-machine-for-tutorial)
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
    (state name:state-name transition:transition-spec ...)
    #:binding (export name)))

(define-syntax-rule
  (define-state-syntax name trans)
  (define-extension name state-macro trans))
]

By adding an extension class called @racket[state-macro] and allowing @racket[state-spec] to be extended by these state macros, transformers wrapped with @racket[state-macro] can be used in @racket[state-spec] positions. syntax-spec provides @racket[define-extension] for defining these wrapped transformers. These macros will be hygienic in our DSL. Since only certain nonterminals are extensible by certain extension classes, we can control what kinds of macros can be used where.

Now let's create a macro in our language!

@examples[#:label #f
(require racket/class
         syntax-spec/tests/dsls/state-machine-for-tutorial)
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
