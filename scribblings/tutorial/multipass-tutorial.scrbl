#lang scribble/manual

@(require (for-label racket racket/block racket/class racket/match racket/list syntax/parse "../../main.rkt")
          scribble/example)
@(define eval (make-base-eval '(require racket (for-syntax racket))))
@(define-syntax-rule (repl body ...) (examples #:eval eval #:label #f body ...))

@title[#:tag "multipass"]{Advanced Tutorial: A Compiler with Transformative Passes}

Many DSLs need a compiler that transforms syntax in several passes. Some passes may just be static checks, and others may actually transform the program, often to a restricted subset of the surface language. When using syntax-spec, some special care needs to be taken with transformative passes. To demonstrate how such a DSL can be implemented, we will create a language with an @hyperlink["https://en.wikipedia.org/wiki/A-normal_form"]{A-normal form} transformation and an unused variable pruning optimization.

@section[#:tag "multipass-expander"]{Expander}

Here is the syntax-spec of our language:

@repl[
#:hidden #t
(module grammar racket
(provide (all-defined-out) (for-space anf (all-defined-out)) (for-syntax (all-defined-out) (for-space anf (all-defined-out))))
  (require "main.rkt" (for-syntax syntax/parse racket))
  (require (for-syntax racket/match racket/syntax racket/list))
(syntax-spec
  (binding-class var
                 #:reference-compiler immutable-reference-compiler)
  (nonterminal full-expr
    #:binding-space anf
    n:number
    x:var
    (let ([x:var e:full-expr]) body:full-expr)
    #:binding (scope (bind x) body)
    (+ a:full-expr b:full-expr)
    (* a:full-expr b:full-expr)
    (/ a:full-expr b:full-expr)
    (rkt e:racket-expr))
  (nonterminal anf-expr
    #:binding-space anf
    ((~datum let) ([x:var e:rhs-expr]) body:anf-expr)
    #:binding (scope (bind x) body)
    e:rhs-expr)
  (nonterminal rhs-expr
    #:binding-space anf
    ((~datum +) a:immediate-expr b:immediate-expr)
    ((~datum *) a:immediate-expr b:immediate-expr)
    ((~datum /) a:immediate-expr b:immediate-expr)
    ((~datum rkt) e:racket-expr)
    e:immediate-expr)
  (nonterminal immediate-expr
    #:binding-space anf
    x:var
    n:number)

  (host-interface/expression
   (eval-expr e:full-expr)
   #'(compile-expr e)))

(begin-for-syntax
  (define local-expand-anf (nonterminal-expander anf-expr)))

(define-syntax compile-expr
  (syntax-parser
    [(_ e)
     ; I chose to use compile-time functions instead of macros because there is a lot
     ; of non-syntax data to pass around. But we still get hygiene with define/hygienic.

     ; need to expand to make sure everything is properly bound
     ; for the analysis pass, which uses symbol tables.
     (define e/anf (local-expand-anf (to-anf #'e) #:should-rename? #t))
     (define e/pruned (prune-unused-variables e/anf))
     ; this last local-expand-anf might be unnecessary for this compiler, but i'll leave it in
     ; since most compilers would need it.
     (define/syntax-parse e/pruned^ (local-expand-anf e/pruned #:should-rename? #t))
     #'(compile-anf e/pruned^)]))

(begin-for-syntax
  ; full-expr -> anf-expr
  (define (to-anf e)
    ; list of (list Identifier rhs-expr)
    ; most recent, and thus innermost, binding first
    (define bindings-rev '())
    ; Identifier rhs-expr -> Void
    ; ends up producing a let-binding of x to e in the result
    (define (lift-binding! x e) (set! bindings-rev (cons (list x e) bindings-rev)))
    (define e^ (to-rhs! e lift-binding!))
    (wrap-lets e^ (reverse bindings-rev)))

  ; full-expr (Identifier rhs-expr -> Void) -> rhs-expr
  ; this doesn't need to be hygienic, only the whole pass.
  ; in other compilers, helpers may need to be hygienic too.
  (define (to-rhs! e lift-binding!)
    (syntax-parse e
      [((~datum let) ([x e]) body)
       (define e^ (to-rhs! #'e lift-binding!))
       (lift-binding! #'x e^)
       (to-rhs! #'body lift-binding!)]
      [(op a b)
       (define/syntax-parse a^ (to-immediate! #'a lift-binding!))
       (define/syntax-parse b^ (to-immediate! #'b lift-binding!))
       #'(op a^ b^)]
      [(~or ((~datum rkt) _)
            x:id
            n:number)
       this-syntax]))

  ; full-expr (Identifier rhs-expr -> Void) -> immediate-expr
  (define (to-immediate! e lift-binding!)
    (syntax-parse e
      [(~or x:id n:number) this-syntax]
      [_
       (define/syntax-parse tmp (generate-temporary 'tmp))
       (define e^ (to-rhs! this-syntax lift-binding!))
       (lift-binding! #'tmp e^)
       #'tmp]))

  ; rhs-expr (listof (list Identifier rhs-expr) )
  (define (wrap-lets e bindings)
    (match bindings
      [(cons binding bindings)
       (with-syntax ([x (first binding)]
                     [rhs (second binding)]
                     [body (wrap-lets e bindings)])
         #'(let ([x rhs]) body))]
      ['() e])))

(begin-for-syntax
  ; anf-expr -> anf-expr
  (define (prune-unused-variables e)
    (define used-vars (get-used-vars e))
    (remove-unused-vars e used-vars))

  ; anf-expr -> SymbolTable
  ; non-hygienic because it's just an analysis pass
  (define (get-used-vars e)
    ; Go bottom-up, seeing references before their binders.
    ; The invariant is that we only traverse expressions that need
    ; to be evaluated.
    ; The innermost expression is needed, so we traverse it. From there,
    ; we only traverse expressions that are (transitively) needed.
    ; If we see a reference, mark it as used.
    ; If we see a binder that is marked as used,
    ; we need its rhs' referenced variables too, so recur on the rhs.
    ; If we see a binder that isn't marked as used, it was never referenced,
    ; so we don't traverse its rhs since it isn't needed.
    (syntax-parse e
      [((~datum let) ([x e]) body)
       (define body-vars (get-used-vars #'body))
       (if (symbol-set-member? body-vars #'x)
           (symbol-set-union body-vars (get-used-vars #'e))
           body-vars)]
      [(op a b)
       (symbol-set-union (get-used-vars #'a) (get-used-vars #'b))]
      [x:id
       (immutable-symbol-set #'x)]
      [((~datum rkt) e)
       (apply immutable-symbol-set (get-racket-referenced-identifiers [var] #'e))]
      [n:number (immutable-symbol-set)]))

  ; anf-expr SymbolTable -> anf-expr
  (define (remove-unused-vars e used-vars)
    (syntax-parse e
      [((~and let (~datum let)) ([x e]) body)
       (define/syntax-parse body^ (remove-unused-vars #'body used-vars))
       (if (symbol-set-member? used-vars #'x)
           ; no need to recur on e since it's not a let
           #'(let ([x e])
               body^)
           #'body^)]
      [_ this-syntax])))

(define-syntax compile-anf
  (syntax-parser
    [(_ ((~datum let) ([x e]) body))
     #'(let ([x (compile-anf e)]) (compile-anf body))]
    [(_ (op a b)) #'(op a b)]
    [(_ ((~datum rkt) e))
     #'(let ([x e])
         (if (number? x)
             x
             (error 'rkt "expected a number, got ~a" x)))]
    [(_ e) #'e]))


)
(require 'grammar "main.rkt" (for-syntax syntax/parse))
]

@racketmod[
racket
(require syntax-spec-dev (for-syntax syntax/parse racket/syntax racket/match racket/list))
(syntax-spec
  (binding-class var
                 #:reference-compiler immutable-reference-compiler)
  (nonterminal full-expr
    #:binding-space anf
    n:number
    x:var
    (let ([x:var e:full-expr]) body:full-expr)
    #:binding (scope (bind x) body)
    (+ a:full-expr b:full-expr)
    (* a:full-expr b:full-expr)
    (/ a:full-expr b:full-expr)
    (rkt e:racket-expr))
  (nonterminal anf-expr
    #:binding-space anf
    ((~datum let) ([x:var e:rhs-expr]) body:anf-expr)
    #:binding (scope (bind x) body)
    e:rhs-expr)
  (nonterminal rhs-expr
    #:binding-space anf
    ((~datum +) a:immediate-expr b:immediate-expr)
    ((~datum *) a:immediate-expr b:immediate-expr)
    ((~datum /) a:immediate-expr b:immediate-expr)
    ((~datum rkt) e:racket-expr)
    e:immediate-expr)
  (nonterminal immediate-expr
    #:binding-space anf
    x:var
    n:number)

  (host-interface/expression
   (eval-expr e:full-expr)
   #'(compile-expr e)))
]

Our language supports arithmetic, local variables, and Racket subexpressions.

We have the following nonterminals:

@itemlist[
@item{@racket[full-expr]: The surface syntax of a program}
@item{@racket[anf-expr]: An expression in A-normal form. Users will not be writing these expressions; the compiler will transform @racket[full-expr]s the user writes into @racket[anf-expr]s.}
@item{@racket[rhs-expr]: An expression which is allowed to be on the right-hand side of a binding pair in an expression when it is in A-normal form. Conceptually, these expressions take at most one "step" of reduction to evaluate. In other words, no nested expressions (except for @racket[rkt] expressions).}
@item{@racket[immediate-expr]: Atomic expressions that can immediately be evaluated.}
]

A-normal form makes the evaluation order of the program completely unambiguous and simplifies compilation to a language like assembly. Now, let's transform our surface syntax to it!

@section{A-normal Form Transformation}

The core idea of transforming to A-normal form is extracting nested sub-expressions into temporary variables. For example:

@racketblock[
(+ (+ 1 2) (+ 3 4))
~>
(let ([tmp1 (+ 1 2)])
  (let ([tmp2 (+ 3 4)])
    (+ tmp1 tmp2)))
]

To follow our grammar for an @racket[anf-expr], the arguments to functions like @racket[+] must be immediate expressions, like variable references or numbers. Our source program did not obey this rule, so we had to create temporary variables for subexpressions and replace each subexpression with a reference to its temporary variable.

Now let's automate this process:

@racketblock[
(begin-for-syntax
  (code:comment2 "full-expr -> anf-expr")
  (code:comment2 "convert an expression to A-normal form")
  (define (to-anf e)
    (define bindings-rev '())
    (code:comment2 "Identifier rhs-expr -> Void")
    (code:comment2 "record a variable binding pair")
    (define (lift-binding! x e) (set! bindings-rev (cons (list x e) bindings-rev)))
    (define e^ (to-rhs! e lift-binding!))
    (wrap-lets e^ (reverse bindings-rev)))

  (code:comment2 "full-expr (Identifier rhs-expr -> Void) -> rhs-expr")
  (code:comment2 "convert an expr to an rhs-expr, potentially recording bindings")
  (define (to-rhs! e lift-binding!)
    (syntax-parse e
      [((~datum let) ([x e]) body)
       (define e^ (to-rhs! #'e lift-binding!))
       (lift-binding! #'x e^)
       (to-rhs! #'body lift-binding!)]
      [(op a b)
       (define/syntax-parse a^ (to-immediate! #'a lift-binding!))
       (define/syntax-parse b^ (to-immediate! #'b lift-binding!))
       #'(op a^ b^)]
      [(~or ((~datum rkt) _)
            x:id
            n:number)
       this-syntax]))

  (code:comment2 "full-expr (Identifier rhs-expr -> Void) -> immediate-expr")
  (code:comment2 "convert a full-expr to an immediate-expr, potentially recording bindings")
  (define (to-immediate! e lift-binding!)
    (syntax-parse e
      [(~or x:id n:number) this-syntax]
      [_
       (define/syntax-parse tmp (generate-temporary 'tmp))
       (define e^ (to-rhs! this-syntax lift-binding!))
       (lift-binding! #'tmp e^)
       #'tmp]))

  (code:comment2 "rhs-expr (Listof (List Identifier rhs-expr)) -> anf-expr")
  (code:comment2 "wrap the innermost expression with `let`s for the bindings that were recorded")
  (define (wrap-lets e bindings)
    (match bindings
      [(cons binding bindings)
       (with-syntax ([x (first binding)]
                     [rhs (second binding)]
                     [body (wrap-lets e bindings)])
         #'(let ([x rhs]) body))]
      ['() e])))
]

Our transformation goes through the expression, recording the temporary variable bindings to lift. The final @racket[rhs-expr] returned by @racket[to-rhs] will be the body of the innermost @racket[let] at the end of the transformation. Converting to an @racket[rhs-expr] or an @racket[immediate-expr] has the side effect of recording a binding pair to be lifted, and the result of replacing complex subexpressions with temporary variable references is returned from each helper.

Notice that the code generation pass is implemented as macro, while the intermediate passes are implemented as compile-time functions. Using a Racket macro for the code generator is convenient because it provide hygiene for any temporary names we introduce. For the intermediate passes we must use compile-time functions rather than macros, for three reasons:

@itemlist[
@item{
The intermediate passes do not generate Racket syntax that can be further expanded by the Racket macro expander. Instead, they generate code in our DSL's intermediate representation.
}
@item{
Compiler passes may need additional arguments and return values, which may not be syntax objects. This is possible with a compile-time function, but not with a macro. For example, our A-normal form transformation receives the @racket[bind!] procedure as an argument.
}
@item{
Compiler passes may use side effects, and rely on a particular order of evaluation. For our A-normal form pass, we want to create @racket[let]-bindings for the innermost subexpressions first. We accomplish this via the way we order calls to the @racket[bind!] procedure.
}
]

@section{Pruning unused variables}

Using syntax-spec's symbol tables and binding operations, we can add an optimizing pass that removes unused variables.

For example:

@racketblock[
(let ([x (+ 2 2)])
  (let ([y (+ 3 3)])
  x))
~>
(let ([x (+ 2 2)])
  x)
]

Since @racket[y] is not referenced, we can just remove its definition from the program. Note that this optimization only makes sense when the right-hand-side of a definition is free of side-effects. For example, pruning @racket[y] in this example would change the behavior of the program:

@racketblock[
(let ([x (+ 2 2)])
  (let ([y (rkt (begin (displayln "hello!") (+ 3 3)))])
  x))
~>
(let ([x (+ 2 2)])
  x)
]

Without pruning, this would print something, but with pruning, it would not. Our optimization shouldn't change the behavior of the program. This DSL is designed with the requirement that @racket[rkt] forms only have pure computations inside, but this cannot easily be checked. As such, we will assume Racket subexpressions are free of side effects, and our optimization will only be sound for side-effect-free Racket subexpressions.

@racketblock[
(begin-for-syntax
  (code:comment2 "anf-expr -> anf-expr")
  (code:comment2 "reconstruct the expression, excluding definitions of unused variables")
  (define (prune-unused-variables e)
    (define used-vars (get-used-vars e))
    (remove-unused-vars e used-vars))

  (code:comment2 "anf-expr -> ImmutableSymbolSet")
  (code:comment2 "compute the set of used variables")
  (define (get-used-vars e)
    (syntax-parse e
      [((~datum let) ([x e]) body)
       (define body-vars (get-used-vars #'body))
       (if (symbol-set-member? body-vars #'x)
           (symbol-set-union body-vars (get-used-vars #'e))
           body-vars)]
      [(op a b)
       (symbol-set-union (get-used-vars #'a) (get-used-vars #'b))]
      [x:id
       (immutable-symbol-set #'x)]
      [(~or ((~datum rkt) _) n:number) (immutable-symbol-set)]))

  (code:comment2 "anf-expr ImmutableSymbolSet -> anf-expr")
  (code:comment2 "reconstruct the expression, excluding definitions of specified unused variables")
  (define (remove-unused-vars e used-vars)
    (syntax-parse e
      [((~and let (~datum let)) ([x e]) body)
       (define/syntax-parse body^ (remove-unused-vars #'body used-vars))
       (if (symbol-set-member? used-vars #'x)
           #'(let ([x e])
               body^)
           #'body^)]
      [_ this-syntax])))
]

@;TODO don't ignore racket subexpression references. Requires fixing a bug though.
First, we figure out which variables are referenced, using a bottom-up traversal. We only include consider variables in the right-hand-side of a @racket[let] used if we have determined that the variable bound by the @racket[let] is used in its body. For now, we ignore references in Racket subexpressions.

Then, with that knowledge, we reconstruct the program, only including bindings for used variables.

This optimization is slightly simplified by having already transformed the program to A-normal form. We can see this in @racket[remove-unused-vars]: We don't need to recur on the right-hand-side of a let-binding because we know there are no variable bindings to be removed from that expression.

@section{Putting it all Together}

Due to the nature of expansion and binding structure, some special care needs to be taken in sequencing multiple transformative compiler passes. Since our A-normal form transformation adds new bindings, we need to re-expand the result so syntax-spec can compute and check binding information for use in later passes/compilation:

@racketblock[
(begin-for-syntax
  (define local-expand-anf (nonterminal-expander anf-expr)))

(define-syntax compile-expr
  (syntax-parser
    [(_ e)
     (define e/anf (local-expand-anf (to-anf #'e) #:should-rename? #t))
     (define e/pruned (prune-unused-variables e/anf))
     (define/syntax-parse e/pruned^ (local-expand-anf e/pruned #:should-rename? #t))
     #'(compile-anf e/pruned^)]))
]

We perform this re-expansion using @racket[nonterminal-expander]. This function expects DSL syntax of a specified nonterminal (here, @racket[anf-expr]) and expands macros in the DSL code, checks binding structure, etc. It's kind of like @racket[local-expand] but for a particular nonterminal. This is what happens in a host interface that produces the expanded, core syntax that your compiler works with. We use @racket[#:should-rename? #t] to ensure that we re-compile and rename identifiers in this expansion.

The expansion after pruning is technically unnecessary for this example since we are only removing bindings in that pass, but it is good to always make sure your compiler is receiving freshly expanding syntax. This extra expansion also makes sure your optimization produces valid syntax. In general, even if your compiler just has a single transformative pass before compilation, you should expand the result of the pass.

An additional caveat is that identifiers need to undergo the same number of expansions for things to work properly. The easiest way to do this is to expand only the entire DSL expression at once, rather than expanding subexpressions individually.

Finally, we must implement compilation of A-normal form expressions to Racket, which is straightforward:

@racketblock[
(define-syntax compile-anf
  (syntax-parser
    [(_ ((~datum let) ([x e]) body))
     #'(let ([x (compile-anf e)]) (compile-anf body))]
    [(_ (op a b)) #'(op a b)]
    [(_ ((~datum rkt) e))
     #'(let ([x e])
         (if (number? x)
             x
             (error 'rkt "expected a number, got ~a" x)))]
    [(_ e) #'e]))
]

@repl[
(eval-expr 1)
(eval-expr (let ([x 1]) (let ([y 2]) x)))
(eval-expr (let ([unused (rkt (displayln "can anyone hear me?"))]) 42))
]

To summarize the key points:

@itemlist[
@item{We used compile-time functions for compiler passes, rather than macros.}
@item{We can have multiple passes in a compiler simply by sequencing compile-time functions that operate on expanded DSL expressions.}
@item{Since we have transformative passes in our compiler, we must re-expand resulting syntax using @racket[nonterminal-expander] after each transformation.}
]
