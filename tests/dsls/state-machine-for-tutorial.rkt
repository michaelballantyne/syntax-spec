#lang racket/base

(provide (all-defined-out))
(require "../../testing.rkt"
         racket/match
         racket/class
         (for-syntax racket/list))

(syntax-spec
  (binding-class event-var)
  (binding-class state-name)
  (extension-class state-macro)

  (nonterminal/exporting state-spec
    #:allow-extension state-macro
    (state name:state-name transition:transition-spec ...)
    #:binding (export name))

  (nonterminal transition-spec
    (on (event-name:id arg:event-var ...) action:action-spec)
    #:binding (scope (bind arg))
    (on (event-name:id arg:event-var ...) #:when guard:racket-expr action:action-spec)
    #:binding (scope (bind arg) guard))

  (nonterminal action-spec
    (goto next-state-name:state-name))

  (host-interface/expression
    (machine #:initial initial-state:state-name s:state-spec ...)
    #:binding (scope (import s) initial-state)
    (check-for-inaccessible-states #'initial-state (attribute s))
    #'(compile-machine initial-state s ...)))

(begin-for-syntax
  ; Identifier (listof Syntax) -> (listof Syntax)
  ; Error if there is an inaccessible state
  (define (check-for-inaccessible-states initial-state-id state-specs)
    (define accessible-states (get-accessible-states initial-state-id state-specs))
    (for/list ([state-spec state-specs]
               #:unless (symbol-set-member? accessible-states (state-spec-name state-spec)))
      (error 'machine "Inaccessible state: ~a" (syntax->datum (state-spec-name state-spec)))))

  ; Identifier (listof Syntax) -> SymbolSet
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

  ; Syntax -> Identifier
  (define (state-spec-name state-spec)
    (syntax-parse state-spec
      [(state name . _) #'name]))

  ; Syntax -> (listof Identifier)
  ; Possible next states
  (define (state-spec-next-state-names state-spec)
    (syntax-parse state-spec
      [(state name
         (on action (~optional (~seq #:when guard))
             (goto next-state-name))
         ...)
       (attribute next-state-name)])))

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

           (new machine%)))]))

(begin-for-syntax
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
                  transition)])))))

(define-syntax-rule
  (define-state-syntax name trans)
  (define-extension name state-macro trans))

(module+ test
  (define mchn
    (machine
     #:initial green
     (state green
            (on (good) (goto green))
            (on (bad) (goto red)))
     (state red
            (on (good) (goto green))
            (on (bad) (goto red)))))
  (check-equal?
   (send mchn get-state)
   'green)
  (send mchn good)
  (check-equal?
   (send mchn get-state)
   'green)
  (send mchn bad)
  (check-equal?
   (send mchn get-state)
   'red)
  (check-exn
   #rx"machine: Inaccessible state: unreachable"
   (lambda ()
     (convert-compile-time-error
      (machine
       #:initial the-initial-state
       (state the-initial-state)
       (state unreachable)))))

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
  (check-equal? (send turnstile get-state)
                'locked)
  (send turnstile coin 0.10)
  (check-equal? (send turnstile get-state)
                'locked)
  (send turnstile coin 0.25)
  (check-equal? (send turnstile get-state)
                'unlocked)
  (send turnstile person-enters)
  (check-equal? (send turnstile get-state)
                'locked)
  (check-exn
   #rx"machine: No transition defined for event 'person-enters in state 'locked"
   (lambda ()
     (send turnstile person-enters)))

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
  (check-equal? (send traffic-light get-state)
                'red)
  (send traffic-light tick)
  (check-equal? (send traffic-light get-state)
                'green)
  (send traffic-light tick)
  (check-equal? (send traffic-light get-state)
                'yellow)
  (send traffic-light tick)
  (check-equal? (send traffic-light get-state)
                'red))
