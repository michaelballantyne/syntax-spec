#lang scribble/manual

@(require (for-label racket racket/block racket/class racket/match racket/list syntax/parse "../../main.rkt")
          scribble/example)

@title{Advanced Tutorial: Simply Typed Lambda Calculus}

This guide demonstrates advanced usage of syntax-spec via the case study of construscting a DSL for the simply typed lambda calculus.

We will:

@itemlist[
@;TODO
]

Here is an example program in our language:

@racketblock[
(let ([f (lambda ([x : Number]) x)])
  (f 1))
]

Let's start out with defining the grammar and binding rules for basic typed expressions:

@racketblock[#:escape unracket
(syntax-spec
  (binding-class typed-var)
  (extension-class typed-macro #:binding-space stlc)
  (nonterminal typed-expr
    #:allow-extension typed-macro
    #:binding-space stlc

    x:typed-var
    n:number

    (#%lambda ([x:typed-var (~datum :) t:type] ...) body:typed-expr)
    #:binding (scope (bind x) body)
    (#%app fun:typed-expr arg:typed-expr ...)

    (#%let ([x:typed-var e:typed-expr] ...) body:typed-expr)
    #:binding (scope (bind x) body)

    (~> (e (~datum :) t)
        #'(: e t))
    (: e:typed-expr t:type)

    (~> (fun arg ...)
        #'(#%app fun arg ...)))
  (nonterminal type
    Number
    ((~datum ->) arg-type:type ... return-type:type))
  (host-interface/expression
   (stlc/expr e:typed-expr)
   (infer-expr-type #'e)
   #'(compile-expr e))
  (host-interface/expression
   (stlc/infer e:typed-expr)
   (define t (infer-expr-type #'e))
   (define t-datum (type->datum t))
   #`'#,t-datum))
]

There are some features we've never seen here. Let's go through them one by one:

@racketblock[
(binding-class typed-var)
(extension-class typed-macro #:binding-space stlc)
(nonterminal typed-expr
    ...

    #:binding-space stlc

    ...)
]

Syntax-spec supports @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{binding spaces}, which allow DSL forms to have the same names as regular Racket forms like @racket[let] without shadowing them. Even DSL macros won't shadow Racket names. We will eventually write a macro for @racket[let] so we don't have to write @racket[#%let] when we use the DSL.

@racketblock[

(nonterminal typed-expr
    ...

    (~> (e (~datum :) t)
        #'(: e t))
    (: e:typed-expr t:type)

    ...)
]

This is called a rewrite production. We have a DSL form, @racket[:], for type annotations like @racket[(: 1 Number)]. We add a rewrite production to allow infix use like @racket[(1 : Number)] for better readability.

We have another rewrite production that converts function applications to @racket[#%app] forms. It is important that this comes after the type annotation rewrite. Otherwise, infix usages would be treated as function applications.

In general, it is a good idea to tag most forms in your grammar like @racket[#%app] to make your compiler less bug-prone. It also allows us to rely on datum literals for distinguishing between forms, which is useful when your form names are in a special binding space.

Now let's define @racket[infer-expr-type]:

@racketblock[
(begin-for-syntax
  (struct number-type [] #:prefab)
  (struct function-type [arg-types return-type] #:prefab)

  (define-persistent-symbol-table types)

  (define (infer-expr-type e)
    (syntax-parse e
      [n:number (number-type)]
      [x:id (get-identifier-type #'x)]
      [((~datum #%lambda) ([x:id _ t] ...) body)
       (define arg-types (map parse-type (attribute t)))
       (for ([x (attribute x)]
             [t arg-types])
         (extend-type-environment! x t))
       (define body-type (infer-expr-type #'body))
       (function-type arg-types body-type)]
      [((~datum #%app) f arg ...)
       (define f-type (infer-expr-type #'f))
       (match f-type
         [(function-type expected-arg-types return-type)
          (unless (= (length expected-arg-types) (length (attribute arg)))
            (raise-syntax-error 'infer-expr-type
                                (format "arity error. expected ~a arguments, but got ~a"
                                        (length expected-arg-types)
                                        (length (attribute arg)))
                                this-syntax))
          (for ([expected-type expected-arg-types]
                [arg (attribute arg)])
            (check-expr-type arg expected-type))
          return-type]
         [_ (raise-syntax-error 'infer-expr-type
                                (format "type mismatch. expected a function type, but got ~a"
                                        (type->datum f-type))
                                #'f)])]
      [((~datum :) e t-stx)
       (define t (parse-type #'t-stx))
       (check-expr-type #'e t)
       t]
      [((~datum #%let) ([x e] ...) body)
       (for ([x (attribute x)]
             [e (attribute e)])
         (extend-type-environment! x (infer-expr-type e)))
       (infer-expr-type #'body)]))

  (define (get-identifier-type x)
    (symbol-table-ref types x (lambda () (raise-syntax-error #f "untyped identifier" x))))

  (define (extend-type-environment! x t)
    (void (symbol-table-ref types x (lambda () (symbol-table-set! types x t)))))

  (define (check-expr-type e expected-type)
    (define actual-type (infer-expr-type e))
    (unless (equal? expected-type actual-type)
      (raise-syntax-error 'infer-expr-type
                          (format "type mismatch. expected ~a, but got ~a"
                                  (type->datum expected-type)
                                  (type->datum actual-type))
                          e)))

  (define (parse-type t-stx)
    (syntax-parse t-stx
      [(~datum Number) (number-type)]
      [((~datum ->) arg-type ... return-type)
       (function-type (map parse-type (attribute arg-type))
                      (parse-type #'return-type))]))

  (define (type->datum t)
    (match t
      [(number-type) 'Number]
      [(function-type arg-types return-type)
       (append (list '->)
               (map type->datum arg-types)
               (list (type->datum return-type)))])))
]

We use prefab structs for our compile-time representation of types and we have a persistent symbol table mapping identifiers to types. A persistent symbol table allows information like an identifier's type to be used between modules even if the providing module has been compiled. Eventually, we'll add definitions to our language, so when type-checking a module that requires a typed identifier, we'll need the identifier's type from the persistent symbol table.

We have to use prefab structs because persistent symbol tables can't persist non-prefab structs. The only values allowed in a symbol table are those which satisfy the @racket[syntax-datum?] predicate.

@racket[extend-type-environment!] uses a bit of a hack. By default, symbol tables error when setting an identifier's value after it has already been set. We will end up re-inferring an expression's type later on, so we use this hack to only set the type if it isn't already set.

@;TODO don't have a hack in the tutorial, it looks bad. add an optional flag or something.

The rest is a typical type checker, nothing syntax-spec-specific.

Now let's implement our compiler:

@racketblock[#:escape unracket
(define-syntax compile-expr
  (syntax-parser
    [(_ n:number) #'n]
    [(_ x:id) #'x]
    [(_ ((~datum #%lambda) ([x:id _ _] ...) body))
     #'(lambda (x ...) (compile-expr body))]
    [(_ ((~datum #%app) f arg ...))
     #'((compile-expr f) (compile-expr arg) ...)]
    [(_ ((~datum :) e _)) #'(compile-expr e)]
    [(_ ((~datum #%let) ([x e] ...) body))
     #'(let ([x (compile-expr e)] ...) (compile-expr body))]))
]

Nothing special here, it's a straightforward translation to Racket. We pretty much just throw away the types.

Finally, we can write macros for @racket[let] and @racket[lambda]:

@racketblock[
(define-syntax define-stlc-syntax
  (syntax-parser
    [(_ name:id trans:expr)
     #'(define-extension name typed-macro trans)]))

(define-stlc-syntax let
  (syntax-parser
    [(_ ([x e] ...) body) #'(#%let ([x e] ...) body)]))

(define-stlc-syntax lambda
  (syntax-parser
    [(_ ([x (~datum :) t] ...) body) #'(#%lambda ([x : t] ...) body)]))

(define-stlc-syntax let*
  (syntax-parser
    [(_ () body) #'(let () body)]
    [(_ ([x:id e] binding ...) body)
     #'(let ([x e]) (let* (binding ...) body))]))
]

Right now, these don't need to be macros. But when we add definitions, We will desugar multi-body @racket[let] and @racket[lambda] expressions to single-body ones.

Now we can run some programs:

@examples[#:label #f
(require syntax-spec/tests/dsls/simply-typed-lambda-calculus)
(stlc/infer 1)
(stlc/expr 1)
(stlc/infer (lambda ([x : Number]) x))
(stlc/expr (lambda ([x : Number]) x))
]

@section{Integrating Racket Expressions}

Let's add arbitrary Racket expressions to our language. These can evaluate to anything, so we can't infer their types. We can require the user to annotate the type, but we shouldn't just trust that the type is correct. Instead, we should add a contract check to ensure that the annotation is accurate.

We also need to add a contract check in the other direction, even if we don't allow arbitrary Racket expressions. Let's consider a program in our language:

@racketblock[
(stlc/expr (lambda ([f : (-> Number Number)] [x : Number]) (f x)))
]

It evaluates to a function which takes in a function and a number and applies the function to a number. But @racket[stlc/expr] gives us a raw procedure that we can pass anything into!

@racketblock[
((stlc/expr (lambda ([f : (-> Number Number)] [x : Number]) (f x)))
 "not a function"
 1)
]

This produces a runtime type error from inside the typed code! This should be impossible. And if we allow DSL variables to be referenced in Racket expressions, we'll need to insert contract checks on references to make sure they're used properly. We can do this by creating a custom reference compiler.

Let's do it!

@racketblock[#:escape unracket
(syntax-spec
  ...

  (nonterminal typed-expr
    ...

    (rkt e:racket-expr (~datum :) t:type)

    ...)

  ...

  (host-interface/expression
    (stlc/expr e:typed-expr)
    (define/syntax-parse t (infer-expr-type #'e))
    #'(compile-expr/top e t))

  ...)

(define-syntax compile-expr/top
  (syntax-parser
    [(_ e t-stx)
     (define t (syntax->datum #'t-stx))
     (define/syntax-parse e^
       #'(with-reference-compilers ([typed-var typed-var-reference-compiler])
           (compile-expr e)))
     #`(contract #,(type->contract-stx t)
                 e^
                 'stlc 'racket
                 #f #'e^)]))

(begin-for-syntax
  (define typed-var-reference-compiler
    (make-variable-like-reference-compiler
     (lambda (x)
       #`(contract #,(type->contract-stx (get-identifier-type x))
                   #,x
                   'stlc 'racket
                   '#,x #'#,x))))

  (define (type->contract-stx t)
    (match t
      [(number-type) #'number?]
      [(function-type arg-types return-type)
       (define/syntax-parse (arg-type-stx ...) (map type->contract-stx arg-types))
       (define/syntax-parse return-type-stx (type->contract-stx return-type))
       #'(-> arg-type-stx ... return-type-stx)])))

(define-syntax compile-expr
  (syntax-parser
    ...

    [(_ ((~datum rkt) e (~datum :) t))
     #`(contract #,(type->contract-stx (parse-type #'t))
                 e
                 'racket 'stlc
                 #f #'e)]

    ...))
]

We added a new form to our language, @racket[rkt], which contains a racket expression and a type annotation. The compilation of this experssion involves a contract check to make sure the value is of the expected type. We also added a contract check in the other direction when a typed value flows out of the host interface and created a custom reference compiler using @racket[make-variable-like-reference-compiler] which inserts a contract check when a DSL variable is referenced in racket. These contract checks ensure typed values (particularly procedures) are used properly in untyped code.

This implementation is far from efficient. Instead of generating the syntax for a contract check everywhere, we should defer to a runtime function and have the type flow into the runtime since it's a prefab struct. We should also avoid inserting a contract check every time a DSL variable is referenced in Racket and just do it once per variable. But for this tutorial, we'll keep it simple.

Let's run some example programs now:


@examples[#:label #f
(require syntax-spec/tests/dsls/simply-typed-lambda-calculus)
(stlc/expr
  (let ([add (rkt + : (-> Number Number Number))])
    (add 1 2)))
(eval:error
 (stlc/expr
   (rkt "not a number" : Number)))
(eval:error
 (stlc/expr
   (let ([add (rkt <= : (-> Number Number Number))])
     (add 1 2))))
(eval:error
 ((stlc/expr (lambda ([f : (-> Number Number)] [x : Number]) (f x)))
  "not a function"
  1))
(eval:error
 (stlc/expr
  (let ([app (lambda ([f : (-> Number Number)] [x : Number]) (f x))])
    (rkt (app "not a function" 1) : Number))))
]

@;TODO fix weird blame location

@;TODO definitions, implicit block in let
@;TODO let* ?
