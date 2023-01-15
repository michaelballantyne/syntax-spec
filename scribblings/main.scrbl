#lang scribble/manual

@(require (for-label racket))

@title{syntax-spec: A Metalanguage for Hosted DSLs}
@;@title{syntax-spec: Declaring DSL syntaxes}
@author+email["Michael Ballantyne" "michael.ballantyne@gmail.com"]


@;-----------------------

@(define (tech/reference str)
   (tech #:doc '(lib "scribblings/reference/reference.scrbl") str))

@(define (seclink/reference sec str)
   (seclink sec #:doc '(lib "scribblings/reference/reference.scrbl") str))
          
@;-----------------------


This package provides a metalanguage for creating hosted DSLs. @deftech{Hosted DSLs} extend
the syntax of Racket with their own grammar and have their own static semantics and compilers.

The metalanguage allows programmers to declare a DSL's grammar, binding rules, and integration points
with Racket. Under the hood it produces a macro expander for the DSL that parses, checks name bindings,
expands DSL macros, and produces syntax in the DSL's core language for compilation.

You can implement conventional macros that do all these same things, but it can take a lot of manual
effort and a deep knowledge of Racket's syntax API. You might find the metalanguage useful when:

@itemlist[
 @item{your DSL has a complex grammar}
 @item{you want to perform static analysis or optimizing compilation}
 @item{you want fragments of DSL programs to interact across separate Racket modules}
 @item{you want your DSL to be macro-extensible}
 ]

@section{Guide}

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

The machine has two states: locked an unlocked. It reacts to two kinds of external events: a coin
with a given value being inserted, and a person passing through the turnstile. Racket code uses
the machine via the @racket[machine-state] and @racket[machine-step] functions. Within the machine,
Racket code is used to implement the guard on the transition to the unlocked state, which checks
that the given coin is a quarter.

@subsection[#:tag "grammar"]{Grammar}

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

@subsection[#:tag "binding"]{Simple binding}

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

@subsection[#:tag "racket"]{Integrating Racket Subexpressions}

@subsection[#:tag "compilation"]{Compilation}

@subsection[#:tag "macros"]{Macros}

The full code for the state machine example is available at
@url{https://github.com/michaelballantyne/syntax-spec/blob/main/test/dsls/statecharts/statecharts.rkt}.


@;-----------------------

@section{Reference}
@require[(for-label syntax-spec)]
@defmodule[syntax-spec]

@defform[(syntax-spec stx-def ...)]

@subsection{Binding classes}

@defsubform[(binding-class id maybe-description maybe-binding-space)
            #:grammar
            [(maybe-description (code:line #:description string-literal)
                                (code:line))
             (maybe-binding-space (code:line #:binding-space space-symbol)
                                  (code:line))]]

@subsection{Extension classes}

@defsubform[(extension-class id maybe-description maybe-binding-space)
            #:grammar
            [(maybe-description (code:line #:description string-literal)
                                (code:line))
             (maybe-binding-space (code:line #:binding-space space-symbol)
                                  (code:line))]]
@subsection{Nonterminals}

@defsubform[(nonterminal id nonterminal-options production ...)]
@defsubform[(nonterminal/nesting id (nested-id) nonterminal-options production ...)]
@defsubform[(nonterminal/two-pass id nonterminal-options production ...)]

@subsubsection{Nonterminal options}

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

@subsubsection{Productions}

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

@subsubsection{Syntax specs}

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


@subsection{Binding specs}


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

@subsection{Interface macros}

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

@subsection{Compilation}

@subsubsection{Interfacing with Racket}

@defidform[#:kind "nonterminal" racket-expr]

@defidform[#:kind "binding class" racket-var]

@defidform[#:kind "extension class" racket-macro]


@subsubsection{Compiling references to DSL bindings within Racket code}

@defform[(with-reference-compilers
             ([binding-class-id reference-compiler-expr] ...)
           body ...+)]

@defthing[immutable-reference-compiler set!-transformer?]

@defthing[mutable-reference-compiler set!-transformer?]


@subsubsection{Symbol tables}

@defform[(define-persistent-symbol-table id)]

@defform[(define-local-symbol-table id)]

@defproc[(syntax-datum? [v any/c]) boolean?]

@defproc[(symbol-table-set! [table any/c]
                            [id identifier?]
                            [v (or/c syntax? syntax-datum?)]) void?]

@defproc[(symbol-table-ref [table any/c] [id identifier?] [failure any/c]) any/c]

