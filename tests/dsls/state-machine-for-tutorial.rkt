#lang racket/base

(require "../../testing.rkt"
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
    #'(compile-machine initial-state s ...)))

(define-syntax compile-machine
  ; TODO handle when not all events are present in all states. should just ignore the event if no transition for it.
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
    [(_ (on (event-name arg ...) (~optional (~seq #:where guard) #:defaults ([guard #'#t]))
          (goto name))
        machine)
     #'(define/public (event-name arg ...)
         (when guard
           (send machine set-state name)))]))

(define mchn
  (machine
   #:initial green
   (state green
          (on (good) (goto green))
          (on (bad) (goto red)))
   (state red
          (on (bad) (goto red)))))

