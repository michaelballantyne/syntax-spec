#lang racket/base

(provide (all-defined-out))
(require "../../testing.rkt"
         racket/match
         racket/class
         (for-syntax racket/list))

(syntax-spec
  (binding-class event-var)
  (binding-class state-name)

  (nonterminal/exporting state-spec
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
  ; TODO handle when not all events are present in all states. should get a clearer error message.
  (syntax-parser
    [(_ initial-state:id
        ((~literal state) state-name evt ...)
        ...)
     (define/syntax-parse (all-events ...) (unique-event-names #'(evt ... ...)))
     #'(with-reference-compilers ([event-var immutable-reference-compiler])
         ; no reference compiler for state names since they shouldn't be referenced in racket expressions.
         (let ()
           (define machine%
             (class object%
               (define state #f)
               (define/public (set-state state%)
                 (set! state (new state% [machine this])))
               (define/public (get-state) (send state get-state))

               (compile-proxy-method all-events state)
               ...

               (send this set-state initial-state)
               (super-new)))

           (define common%
             (class object%
               (init-field machine)
               (super-new)))

           (define state-name
             (class common%
               (inherit-field machine)

               (define/public (get-state) 'state-name)

               (compile-event-method evt machine)
               ...

               (super-new)))
           ...

           (new machine%)))]))

(begin-for-syntax
  (define (unique-event-names evt-stxs)
    (remove-duplicates (map event-name (syntax->list evt-stxs))
                       free-identifier=?))

  (define (event-name e)
    (syntax-parse e
      [(on (name . _) . _) #'name])))

(define-syntax compile-proxy-method
  (syntax-parser
    [(_ name target)
     #'(define/public (name . args)
         (send/apply target name args))]))

(define-syntax compile-event-method
  (syntax-parser
    #:datum-literals (on ->)
    [(_ (on (event-name arg ...) (~optional (~seq #:when guard) #:defaults ([guard #'#t]))
          (goto name))
        machine)
     #'(define/public (event-name arg ...)
         (when guard
           (send machine set-state name)))]))

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

  #;(define turnstile
    (machine
     #:initial locked

     (state locked
            (on (coin value) #:when (= value 0.25)
                (goto unlocked))
            (on (coin value)
                (goto locked)))

     (state unlocked
            (on (person-enters)
                (goto locked))))))
