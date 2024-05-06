#lang racket/base

(provide (rename-out [define/command-line-options/surface define/command-line-options])
         (for-syntax flag-names (rename-out [arg-spec-cls arg-spec]))
         (except-out (all-defined-out)
                     define/command-line-options))

(require "../../../testing.rkt"
         racket/list
         (only-in racket/cmdline parse-command-line)
         ee-lib/define

         (for-syntax
          racket/base
          (rename-in syntax/parse [define/syntax-parse def/stx])
          syntax/stx
          (except-in ee-lib racket-var)
          racket/list
          racket/sequence
          ))

(begin-for-syntax
  (define-syntax-class arg-spec-cls
    #:attributes [name parser]
    (pattern name:id
             #:attr parser #'identity/p)
    (pattern [name:id parser]))

  (define-syntax-class flag-name
    #:description #f
    (pattern _:string
             #:cut
             #:fail-unless
             (regexp-match? #rx"^([-+][^-+]$|(--|[+][+])[^-+])" (syntax-e this-syntax))
             "bad flag string"
             #:fail-when
             (regexp-match? #rx"^[-+][0-9]$" (syntax-e this-syntax))
             "number flag not allowed"
             #:fail-when
             (regexp-match? #rx"^(-h|--help)$" (syntax-e this-syntax))
             "pre-defined flag not allowed"))

  (define-syntax-class flag-names
    #:description #f
    #:attributes [names]
    (pattern s:flag-name
             #:attr names #'(s))
    (pattern (s:flag-name ...+)
             #:attr names #'(s ...))))

(syntax-spec
  (extension-class option-macro)
  (extension-class flag-macro)

  (nonterminal option
    #:allow-extension option-macro
    (choice/o #:required f:flag ...+)
    (choice/o #:default default-expr:racket-expr f:flag ...+)
    (multi/o init:expr f:flag ...+))

  (nonterminal flag
    #:allow-extension flag-macro
    ((~literal begin) f:flag ...+)
    [names:flag-names arg:arg-spec ... desc:string e:racket-expr]
    #:binding {(recursive arg) e})

  (nonterminal/exporting arg-spec
    (~> name:id #'[name identity/p])
    [name:racket-var parser:racket-expr]
    #:binding (export name))

  (nonterminal/exporting maybe-arg-spec
    (~datum #f)
    arg:arg-spec
    #:binding (re-export arg))

  (host-interface/definition
   (define/command-line-options
     name-expr:racket-expr
     argv-expr:racket-expr
     ([option-name:racket-var opt:option] ...)
     (arg:arg-spec ...)
     rest:maybe-arg-spec)
   #:binding [(export option-name) (re-export arg rest)]
   #:lhs [#:with ([arg-name _] ...) (attribute arg)
          #:attr rest-name (syntax-parse (attribute rest) [[rest-name _] #'rest-name] [_ #f])
          #'(option-name ... arg-name ... (~? rest-name))]
   #:rhs [#:with ([arg-name arg-type] ...) (attribute arg)
          #:attr rest-name (syntax-parse (attribute rest) [[rest-name _] #'rest-name] [_ #f])
          #:attr rest-type (syntax-parse (attribute rest) [[_ rest-type] #'rest-type] [_ #f])
          (define option-keys (for/list ([o (in-syntax #'(option-name ...))]) (gensym (syntax-e o))))
          (def/stx table-expr (compile-table-expr (syntax->list #'(opt ...)) option-keys))
          (def/stx finish-proc-expr
            (compile-finish-proc-expr
             (syntax->list #'(opt ...)) option-keys
             (syntax->list #'(arg-name ...)) (syntax->list #'(arg-type ...))
             (attribute rest-type)))
          (def/stx arg-help-strs-expr
            (compile-arg-help (syntax->list #'(arg-name ...)) (attribute rest-name)))
          #'(let ([name name-expr])
              (parameterize ([current-program-name name])
                (parse-command-line
                 name
                 argv-expr
                 table-expr
                 finish-proc-expr
                 arg-help-strs-expr)))]))

(define-syntax define/command-line-options/surface
  (syntax-parser
    [(_ (~optional (~seq #:program name-expr:expr))
        (~optional (~seq #:argv argv-expr:expr))
        (~optional (~seq #:options option-bindings:expr ...))
        (~optional (~seq #:arguments positional-bindings:expr ...))
        (~optional (~seq #:rest rest-arg:expr)))
     #'(define/command-line-options
         (~? name-expr (find-system-path 'run-file))
         (~? argv-expr (current-command-line-arguments))
         (~? (option-bindings ...) ())
         (~? (positional-bindings ...) ())
         (~? rest-arg #f))]))

(define-syntax-rule
  (define-option-syntax name trans)
  (define-syntax name (option-macro trans)))
(define-syntax-rule
  (define-flag-syntax name trans)
  (define-syntax name (flag-macro trans)))

; compiler

(begin-for-syntax
  (define (identifier->string-literal id) #`#,(symbol->string (syntax-e id)))

  (define (initial-option-value-stx option-stx)
    (syntax-parse option-stx
      [((~literal choice/o) #:required . _)
       #'unset]
      [((~literal choice/o) #:default default-expr . _)
       #'default-expr]
      [((~literal multi/o) init-expr . _)
       #'init-expr]))

  (define-syntax-class exp-flag-spec
    (pattern [[name:string ...] ([arg:id arg-spec] ...) desc:string e]))

  (define ((compile-flag key type) flag-stx)
    (syntax-parse flag-stx
      [[names:flag-names [arg:id arg-p] ... desc:string e]
       (def/stx hash-op (case type [(choice) #'hash-set] [(multi) #'hash-update]))
       (def/stx (arg-name-str-e ...) (stx-map identifier->string-literal #'(arg ...)))
       (def/stx (name ...) #'names.names)
       (def/stx fn
         #`(lambda (flag-name arg ...)
             (let ([arg (run-parser arg-p
                                    (format "<~a> argument to flag ~a" arg-name-str-e flag-name)
                                    arg)] ...)
               (lambda (acc) (hash-op acc '#,key e)))))
       #``[(name ...) ,fn (desc #,@(stx-map identifier->string-literal #'(arg ...)))]]))

  (define (splice-begins exprs)
    (foldr (lambda (expr acc)
             (syntax-parse expr
               [((~literal begin) e ...) (append (splice-begins (attribute e)) acc)]
               [_ (cons this-syntax acc)]))
           '()
           exprs))

  (define (compile-option option-stx key)
    (syntax-parse option-stx
      [((~literal choice/o) (~or #:required (~seq #:default default-expr)) flag ...)
       #`(list 'once-any #,@(map (compile-flag key 'choice) (splice-begins (attribute flag))))]
      [((~literal multi/o) init-expr flag ...)
       #`(list 'multi #,@(map (compile-flag key 'multi) (splice-begins (attribute flag))))]))

  (define (compile-table-expr option-specs option-keys)
    #`(list #,@(map compile-option option-specs option-keys)))

  (define (compile-finish-proc-expr option-specs option-keys arg-names arg-parsers maybe-rest-parser)
    (define required-option-keys+names
      (for/fold ([acc (hash)])
                ([spec option-specs]
                 [key option-keys])
        (syntax-parse spec
          [((~literal choice/o) #:required [flag:flag-names . _] ...)
           (hash-set acc key (apply append (syntax->datum #'(flag.names ...))))]
          [_ acc])))

    (define maybe-parse-rest
      (or maybe-rest-parser #'#f))

    (define arg-names-exprs
      (for/list ([arg arg-names])
        (identifier->string-literal arg)))

    #`(make-finish-proc '#,option-keys
                        (list #,@(map initial-option-value-stx option-specs))
                        '#,required-option-keys+names
                        '#,arg-names-exprs
                        (list #,@arg-parsers)
                        #,maybe-parse-rest))

  (define (compile-arg-help arg-names maybe-rest-name)
    #`'#,(append (map symbol->string (map syntax-e arg-names))
                 (if maybe-rest-name (list (symbol->string (syntax-e maybe-rest-name))) '()))))
; end compiler begin-for-syntax

; runtime
(define identity/p (lambda (x) x))
(define unset
  (let () (struct unset ()) (unset)))

; TODO: I'd like errors to also mention the argument and flag names, as appropriate
(define current-program-name (make-parameter #f))
(define (run-parser parser context arg)
  (define (rewrite-message e)
    (raise-user-error (format "~a: ~a\n  in ~a" (current-program-name) (exn-message e) context)))
  (with-handlers ([exn:fail:user? rewrite-message])
    (parser arg)))

(define (make-finish-proc option-keys initial-option-values required-option-keys
                          arg-names arg-parsers maybe-parse-rest)
  (procedure-reduce-arity
   (lambda (option-procs . args)
     (define-values (positional-args rest-args) (split-at args (length arg-parsers)))

     (define initial-option-map
       (make-immutable-hash (map cons option-keys initial-option-values)))
     (define final-option-map
       (for/fold ([option-map initial-option-map])
                 ([option-proc option-procs])
         (option-proc option-map)))

     (for ([(key flags) required-option-keys])
       (when (eq? (hash-ref final-option-map key) unset)
         (raise-user-error
          (format "~a: one of these flags must be specified: ~a" (current-program-name) flags))))

     (define options
       (for/list ([key option-keys])
         (hash-ref final-option-map key)))

     (define positionals
       (for/list ([arg positional-args]
                  [name arg-names]
                  [parse arg-parsers])
         (run-parser parse (format "positional argument <~a>" name) arg)))

     (define maybe-rest
       (if maybe-parse-rest
           (list (map (lambda (arg) (run-parser maybe-parse-rest "variadic arguments" arg))
                      rest-args))
           '()))

     (apply values
            (append options
                    positionals
                    maybe-rest)))
   (if maybe-parse-rest
       (arity-at-least (+ 1 (length arg-parsers)))
       (+ 1 (length arg-parsers)))))
