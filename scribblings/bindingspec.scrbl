#lang scribble/manual

@(require (for-label racket))

@title{Hosted syntaxes}
@author+email["Michael Ballantyne" "michael.ballantyne@gmail.com"]


@;-----------------------

@(define (tech/reference str)
   (tech #:doc '(lib "scribblings/reference/reference.scrbl") str))

@(define (seclink/reference sec str)
   (seclink sec #:doc '(lib "scribblings/reference/reference.scrbl") str))
          
@;-----------------------

@section{Reference}
@require[(for-label bindingspec)]
@defmodule[bindingspec]

@defform[(define-hosted-syntaxes stx-def ...)]

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
@defsubform[(nesting-nonterminal id (nested-id) nonterminal-options production ...)]
@defsubform[(two-pass-nonterminal id nonterminal-options production ...)]

@subsubsection{Nonterminal options}

@racketgrammar*[(nonterminal-options (code:line maybe-description
                                                maybe-bind-literal-set
                                                maybe-allow-extension
                                                maybe-binding-space))
                (maybe-description (code:line #:description string-literal)
                                   (code:line))
                (maybe-bind-literal-set (code:line #:bind-literal-set literal-set-id)
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
                             spec-variable-id:syntax-class-id)
                (maybe-space (code:line #:space space-name)
                             (code:line))]


@subsection{Binding specs}


@racketgrammar[#:literals (bind recursive re-export export nest nest-one host)
               binding-spec spec-variable-id
               (bind spec-variable-id ...+)
               {spec ...}
               [spec ...]
               (nest spec-variable-id binding-spec)
               (nest-one spec-variable-id binding-spec)
               (recursive spec-variable-id ...+)
               (export spec-variable-id ...+)
               (re-export spec-variable-id ...+)
               (host spec-variable-id)]

@subsection{Interface macros}

@defform[(define-host-interface/expression
           (id . syntax-spec)
           maybe-binding-spec
           pattern-directive ...
           body ...+)]

@defform[#:literals (-> define)
         (define-host-interface/definition
           (id . syntax-spec)
           maybe-binding-spec
           ->
           (define
             [pattern-directive ...
              body ...+]
             [pattern-directive ...
              body ...+]))]

@defform[(define-host-interface/definitions
           (id . syntax-spec)
           maybe-binding-spec
           pattern-directive ...
           body ...+)]

@subsection{Compilation}


@subsubsection{Compiling host sub-terms}

@defform[(resume-host-expansion stx maybe-reference-compilers)
         #:grammar ([maybe-reference-compilers
                     (code:line #:reference-compilers ([binding-class-id transformer] ...))
                     (code:line)])
         #:contracts ([stx syntax?] [transformer (or/c set!-transformer? (-> syntax? syntax?))])]

@subsubsection{Persistent free-identifier tables}

@require[(for-label syntax/id-table)]


@defform[(define-persistent-free-id-table id)]

@defproc[(persistent-free-id-table? [v any/c]) boolean?]

@defproc[(syntax-datum? [v any/c]) boolean?]

@defproc[(identifier-with-binding? [v any/c]) boolean?]

@defproc[(persistent-free-id-table-has-key? [table persistent-free-id-table?] [id identifier-with-binding?]) boolean?]

@defproc[(persistent-free-id-table-set! [table persistent-free-id-table?]
                                        [id identifier-with-binding?]
                                        [v (or/c syntax? syntax-datum?)]) void?]

@defproc[(persistent-free-id-table-ref [table persistent-free-id-table?] [id identifier-with-binding?] [failure any/c]) any/c]


@subsubsection{Compiling bindings and references}

@defthing[compiled-ids persistent-free-id-table?]

@defproc[(compile-binder! [id identifier?] [#:table table (or/c mutable-free-id-table? persistent-free-id-table?) compiled-ids]) identifier?]

@defproc[(compile-binders! [ids (listof identifier?)] [#:table table (or/c mutable-free-id-table? persistent-free-id-table?) compiled-ids]) (listof identifier?)]

@defproc[(compile-reference [id identifier?] [#:table table (or/c free-id-table? persistent-free-id-table?) compiled-ids]) identifier?]

@defthing[immutable-reference-compiler set!-transformer?]

@defthing[mutable-reference-compiler set!-transformer?]

@subsubsection{Binding spaces}

@defproc[(in-space [space symbol?]) (-> syntax? syntax?)]

@defform[(~space-literal id space-symbol)]
