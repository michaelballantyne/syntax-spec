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
                (form-production (code:line (form-id . syntax-spec) maybe-binding-spec))
                (syntax-production (code:line syntax-spec maybe-binding-spec))
                (maybe-binding-spec (code:line #:binding binding-spec))]

@subsubsection{Syntax specs}

@racketgrammar[syntax-spec
               ()
               keyword
               ...
               ...+
               (syntax-spec . syntax-spec)
               spec-variable-id:binding-class-id
               spec-variable-id:nonterminal-id
               spec-variable-id:syntax-class-id]


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

@subsection{Compilation}

@require[(for-label syntax/id-table)]


@defproc[(compile-reference [table free-id-table?] [id identifier?]) identifier?]

@defproc[(compile-binder! [table mutable-free-id-table?] [id identifier?]) identifier?]

@defform[(with-reference-compilers ([binding-class-id transformer-e] ...)
           body ...+)]

@defproc[(resume-host-expansion [stx syntax?]) syntax?]

@defform[(~space-literal id space-symbol)]