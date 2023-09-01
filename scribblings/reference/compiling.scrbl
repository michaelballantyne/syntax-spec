#lang scribble/manual

@(require (for-label racket "../../main.rkt"))

@;-----------------------

@(define (tech/reference str)
   (tech #:doc '(lib "scribblings/reference/reference.scrbl") str))

@(define (seclink/reference sec str)
   (seclink sec #:doc '(lib "scribblings/reference/reference.scrbl") str))

@;-----------------------

@title{Compiling languages}

@section{Compiling references to DSL bindings within Racket code}

@defform[(with-reference-compilers
             ([binding-class-id reference-compiler-expr] ...)
           body ...+)]

@defthing[immutable-reference-compiler set!-transformer?]

@defthing[mutable-reference-compiler set!-transformer?]


@section{Symbol tables}

@defform[(define-persistent-symbol-table id)]

@defform[(define-local-symbol-table id)]

@defproc[(syntax-datum? [v any/c]) boolean?]

@defproc[(symbol-table-set! [table any/c]
                            [id identifier?]
                            [v (or/c syntax? syntax-datum?)]) void?]

@defproc[(symbol-table-ref [table any/c] [id identifier?] [failure any/c]) any/c]

