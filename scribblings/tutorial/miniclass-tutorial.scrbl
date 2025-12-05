#lang scribble/manual

@(require (for-label racket racket/block racket/class racket/match racket/list syntax/transformer syntax/parse "../../main.rkt")
          scribble/example)
@(define eval (make-base-eval '(require racket racket/stxparam syntax/transformer (for-syntax racket syntax/transformer))))
@(define-syntax-rule (repl body ...) (examples #:eval eval #:label #f body ...))
@repl[
#:hidden #t
(require "tests/dsls/miniclass/class.rkt")
]

@title[#:tag "miniclass"]{Advanced Tutorial: Re-interpreting Racket Syntax}

It is possible to create a DSL that re-interprets Racket forms like @racket[define] to behave differently in the context of that DSL. In Racket's @racket[class] DSL, procedure definitions are interpreted as method definitions. syntax-spec makes it easier to create these DSLs, but some special care and knowledge is required. In this tutorial, we'll create a small version of Racket's @racket[class] DSL.

Here is an example of using the language we'll build:

@repl[
(define posn%
  (class
    (field x y)
    (define (scale k) (new posn% (* x k) (* y k)))
    (define (displayln) `(posn ,x ,y))))
(define p (new posn% 1 2))
(send (send p scale 3) displayln)
]

@section[#:tag "miniclass-expander"]{Expander}

Let's start by defining the grammar:

@racketmod[
racket
(require syntax-spec-dev
         racket/stxparam
         (for-syntax racket/list
                     syntax/parse
                     syntax/transformer))

(begin-for-syntax
  (define-syntax-class lambda-id
    (pattern (~or (~literal lambda) (~literal #%plain-lambda)))))

(syntax-spec
  (binding-class method-name
    #:reference-compiler method-reference-compiler)
  (binding-class field-name
    #:reference-compiler field-reference-compiler)

  (nonterminal/exporting class-form
    #:allow-extension racket-macro
    (field name:field-name ...)
    #:binding [(export name) ...]
    ((~literal define-values) (m:method-name)
      (lambda:lambda-id (arg:racket-var ...) body:racket-body ...))
    #:binding [(export m) (scope (bind arg) ... (import body) ...)]

    ((~literal define-syntaxes) (x:racket-macro ...) e:expr)
    #:binding (export-syntaxes x ... e)

    ((~literal begin) e:class-form ...)
    #:binding [(re-export e) ...]

    e:racket-expr))
]

We create separate binding classes for method and field names so that we can transform references to methods and fields differently when they appear in the class body.

@margin-note{
We could allow arbitrary Racket definitions in the class body using @racket[racket-body] instead of @racket[racket-expr]. However, this would require a more sophisticated compilation strategy to allow methods to close over these definitions.
}

The @racket[class-form] nonterminal is for forms that will appear in the class body. We support field declarations, method definitions, macro definitions, @racket[begin], and arbitrary Racket expressions. The Racket expressions will run once, in the constructor.

@margin-note{
 Racket's @racket[class] form accepts more complex expressions on the right-hand-side of @racket[define-values]. We could approach this feature by creating a macro-extensible @racket[method-procedure] nonterminal. Unfortunately, @racket[syntax-spec] is missing features required for expansions in this context. @hyperlink["https://github.com/michaelballantyne/syntax-spec/issues/91"]{Issue}
}

The key piece that allows us to re-interpret Racket syntax is the @racket[racket-macro] @tech[#:key "extension classes"]{extension class}. This extension class allows any definition form that eventually translates to @racket[define-values] and @racket[define-syntaxes] to be used in our DSL. This includes @racket[define] and @racket[define-syntax], but also fancy ones like @racket[define/match]. We do require that @racket[define-values] forms have a @racket[lambda] on the right-hand-side so that we can parse the method arguments and body.

As an example, let's think about how a method definition might expand:

@racketblock[
(define (add2 x) (+ x 2))
~>
(define-values (add2) (lambda (x) (+ x 2)))
]

This is exactly what our method production looks for.

@section{Procedural Layer}

Next, let's think about how we will represent classes at runtime. We'll define two data types:

@racketblock[
(code:comment2 "A ClassInfo is a")
(struct class-info [methods constructor])
(code:comment2 "where")
(code:comment2 "methods is a (Hash Symbol Procedure)")
(code:comment2 "constructor is a (Any ... -> Object)")

(code:comment2 "An Object is a")
(struct object [fields class])
(code:comment2 "where")
(code:comment2 "fields is a (MutableVectorOf Any)")
(code:comment2 "class is a ClassInfo")
]

A @racket[class-info] represents a class itself. @racket[methods] is a hash table mapping method names (symbols) to method implementations. Methods will be implemented as functions whose first argument is @racket[this] and subsequent arguments are the explicit arguments to the method. @racket[constructor] takes in an argument for each field and returns an instance of the class, which is an @racket[object].

A @racket[object] represents an instance of a class. It has its fields as a vector of values (in the same order as the constructor), and the @racket[class-info] containing its methods.

Methods always access the instance via @racket[this]. So we can create one set of procedures in the @racket[class-info] structure and share it across all instances. But fields are instance-specific, so each @racket[object] needs its own. Fields are only accessed from within the class so we can transform each field access to a vector access at an index corresponding to the field. Methods may be referenced inside the class, but they may also be referenced externally via @racket[send] with a dynamically constructed symbol. Thus, we need to be able to access methods by their symbolic name.

Now that we've defined our data types, we can implement functionality for constructing objects and calling methods:

@racketblock[
(define (new cls . fields)
  (apply (class-info-constructor cls) fields))

(define-syntax send
  (syntax-parser
    [(_ obj:expr method-name:id arg:expr ...)
     #'(send-rt obj 'method-name (list arg ...))]
    [(_ obj:expr method-name:id . args)
     #'(send-rt obj 'method-name args)]))

(define (send-rt obj method-name args)
  (let* ([cls (object-class obj)]
         [method (hash-ref (class-info-methods cls) method-name (lambda () (error 'send "unknown method ~a" method-name)))])
    (apply method obj args)))
]

Now let's think about how we will compile the main class form. Recall our example:

@repl[
(define posn%
  (class
    (field x y)
    (define (scale k) (new posn% (* x k) (* y k)))
    (define (displayln) `(posn ,x ,y))))
(define p (new posn% 1 2))
(send (send p scale 3) displayln)
]

We would like that to compile to something like this:

@repl[
(define posn%
  (class-info (hash 'scale (lambda (this-arg k)
                             (syntax-parameterize ([this (make-variable-like-transformer #'this-arg)])
                               (new posn% (* (vector-ref (object-fields this) 0) k)
                                          (* (vector-ref (object-fields this) 1) k))))
                    'displayln (lambda (this-arg)
                                 (syntax-parameterize ([this (make-variable-like-transformer #'this-arg)])
                                   `(posn ,(vector-ref (object-fields this) 0) 
                                          ,(vector-ref (object-fields this) 1)))))
              (lambda (x y) (object (vector x y) posn%))))
(define p (new posn% 1 2))
(send-rt (send-rt p 'scale (list 3)) 'displayln (list))
]

We have to create a dynamic mapping from method names to procedures and a static association between fields and vector indices. We also have to make sure method procedures have an implicit @racket[this-arg] argument for taking in the instance, and set up @racket[this] to refer to @racket[this-arg] in the body of the method.

This is a bit of a simplification, but gives us the general idea of how classes will compile.

@section[#:tag "miniclass-compiler"]{Compiler}

Now let's implement this compilation.

@racketblock[
(syntax-spec
  (host-interface/expression
    (class e:class-form ...)
    #:binding (scope (import e) ...)
    (define-values (defns fields constructor-body) (group-class-decls (splice-begins (attribute e))))
    (compile-class-body defns fields constructor-body)))
]

First, we splice @racket[begin]s so we get a flat list of class-level forms (field declarations, method definitions, and Racket expressions). Then, we group these class-level forms based on their type. Finally, we compile these forms to Racket.

Here is how we splice @racket[begin]s:

@racketblock[
(begin-for-syntax
  (code:comment2 "splices begins (recursively), returns flattened list of exprs.")
  (define (splice-begins exprs)
    (syntax-parse exprs
      [() this-syntax]
      [(expr . rest-exprs)
       (syntax-parse #'expr
         #:literals (begin)
         [(begin e ...)
          (splice-begins (append (attribute e) #'rest-exprs))]
         [_ (cons this-syntax (splice-begins #'rest-exprs))])])))
]

We just flatten everything into a list of class-level forms.

Here is the code for grouping up class-level forms:

@racketblock[
(begin-for-syntax
  (define (group-class-decls exprs)
    (syntax-parse exprs
      #:literals (define-values define-syntaxes field)
      [((~alt (~and defn (define-values . _))
              (~and stx-defn (define-syntaxes . _))
              (field field-name ...)
              constructor-body)
        ...)
       ;; discard stx-defn because syntax definitions don't end up in the generated code
       (values (attribute defn)
               #'(field-name ... ...)
               (attribute constructor-body))])))
]

It's just straightforward syntax manipulation, taking advantage of @racket[syntax-parse]'s powerful patterns.

For compilation, we start with the method reference compilers:

@racketblock[
#:escape unracket
(begin-for-syntax
  (define method-reference-compiler
    (make-variable-like-reference-compiler
     (syntax-parser
       [name:id
        #'(lambda args (send this name . args))]))))
]

Inside of the @racket[class] body, if you reference a method directly, the reference expands to a procedure that invokes the method via @racket[send].

@racketblock[
#:escape unracket
(begin-for-syntax
  (define field-index-table (local-symbol-table))

  (define field-reference-compiler
    (make-variable-like-reference-compiler
     (syntax-parser
       [name:id
        (let ([idx (symbol-table-ref field-index-table #'name)])
          #`(vector-ref (object-fields this) #,idx))])
     (syntax-parser
       [(_ name:id rhs)
        (let ([idx (symbol-table-ref field-index-table #'name)])
          #`(vector-set! (object-fields this) #,idx rhs))]))))
]

Field references access or mutate the object's field vector. We use a symbol table to map field names to indices. This table will be used across all classes, which is safe due to hygiene. We use a local symbol table rather than a persistent one since fields can only be referenced from within the class definition, which means we don't need the table entries to persist across separate compilations.

This table is populated in @racket[compile-constructor], which we'll look at soon. For now, let's start compiling the class body:

@racketblock[
#:escape unracket
(begin-for-syntax
  (define (compile-class-body defns fields constructor-body)
    (syntax-parse (list defns fields constructor-body)
      #:literals (define-values field)
      [(((define-values (method-name:id) (_ (method-arg:id ...) method-body:expr ...)) ...)
        (field-name:id ...)
        (constructor-body ...))
       (define/syntax-parse method-table (compile-methods (attribute method-name) (attribute method-arg) (attribute method-body)))
       (define/syntax-parse constructor-procedure (compile-constructor (attribute field-name) #'cls (attribute constructor-body)))
       #'(letrec ([methods method-table]
                  [constructor constructor-procedure]
                  [cls (class-info methods constructor)])
           cls)])))
]

We generate syntax that creates the method table, constructor procedure, and class info in a letrec. We need recursion because the constructor procedure returns an object with class info @racket[cls].

Now let's see how we compile methods:

@racketblock[
(begin-for-syntax
  (define (compile-methods method-name method-arg method-body)
    (check-duplicate-method-names method-name)
    (syntax-parse (list method-name method-arg method-body)
      [((method-name ...) ((method-arg ...) ...) ((method-body ...) ...))
       #'(make-immutable-hash
          (list
           (cons 'method-name
                 (lambda (this-arg method-arg ...)
                   (syntax-parameterize ([this (make-variable-like-transformer #'this-arg)])
                     method-body
                     ...)))
           ...))]))

  (define (check-duplicate-method-names names)
    (let ([duplicate (check-duplicates names #:key syntax->datum)])
      (when duplicate
        (raise-syntax-error #f "a method with same name has already been defined" duplicate)))))
]

The method table is a mapping from method name to procedure. We set up @racket[this] to refer to the first argument, which is the instance of the class, and the rest of the arguments are those passed in from @racket[send].
@;TODO is it safe to use the symbols? Are they actually renamed the right way? Or do you have to gensym.

We treat method names as symbols to support dynamic dispatch. Symbols are non-hygienic, so we need to do a duplicate method name check on the symbolic names of methods.

Now let's see how the constructor is compiled:

@racketblock[
(define (compile-constructor field-name cls constructor-body)
  (for ([field-name field-name]
        [field-index (in-naturals)])
    (symbol-table-set! field-index-table field-name field-index))
  (syntax-parse (list field-name cls constructor-body)
    [((field-name ...) cls (constructor-body ...))
     #'(lambda (field-name ...)
         (let ([this-val (object (vector field-name ...) cls)])
           (syntax-parameterize ([this (make-variable-like-transformer #'this-val)])
             ;; ensure body is non-empty
             (void)
             constructor-body
             ...)
           this-val))]))
]

The constructor takes in values for the fields, creates an instance and binds it to @racket[this], runs the class-level Racket expressions, and finally returns the instance.

We also associate field names with their vector indices according to their declaration order for @racket[field-reference-compiler].

Finally, let's define the syntax parameter for @racket[this]:

@racketblock[
(define-syntax-parameter this
  (make-expression-transformer
   (syntax-parser
     [_ (raise-syntax-error 'this "used outside of a class" this-syntax)])))
]

That's it. We now have a simple class DSL. To summarize the key points:

@itemlist[
  @item{We have productions with literals of expanded Racket forms to detect definitions and re-interpret them.}
  @item{We use @racket[#:allow-extension racket-macro] to expand any Racket definitions down to @racket[define-values] and @racket[define-syntaxes].}
  @item{We use @racket[racket-body] for Racket definitions or expressions, and @racket[racket-expr] for just expressions.}
  @item{We use a syntax parameter for @racket[this], which gets set in the compiler to refer to the instance. But when used outside of a class body, it is a syntax error.}
]
