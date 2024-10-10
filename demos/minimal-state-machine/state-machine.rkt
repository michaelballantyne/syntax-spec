#lang racket

(provide machine state on on-enter)

(require "../../main.rkt")

(syntax-spec
  (binding-class state-name)
  (binding-class event-var)

  (host-interface/expression
    (machine #:initial-state s:state-name d:machine-decl ...)
    #:binding (scope (import d) ... s)
    #'(compile-machine s d ...))
  
  (nonterminal/exporting machine-decl
    (state n:state-name
      e:event-decl ...)
    #:binding (export n))
  
  (nonterminal event-decl
    (on-enter e:racket-expr ...)
    (on (evt:id arg:event-var ...)
      e:racket-expr ...
      ((~datum ->) s:state-name))
    #:binding (scope (bind arg) ... e ...)))

(require syntax/parse/define (for-syntax syntax/parse racket/list))

(define-syntax compile-machine
  (syntax-parser
    #:datum-literals (machine state on-enter)
    [(_ initial-state
        (state state-name
          (on-enter action ...)
          e ...)
        ...)
     #'(with-reference-compilers ([event-var mutable-reference-compiler])
         (let ()
           (define machine%
             (class object%
               (define state #f)
               (define/public (set-state state%)
                 (set! state (new state% [machine this])))

               (compile-proxy-methods (e ... ...) state)

               (send this set-state initial-state)
               (super-new)))

           (define state-name
             (class object%
               (init-field machine)
               action ...
               (compile-event-method e machine) ...
               (super-new)))
           ...

           (new machine%)))]))

(define-syntax compile-proxy-methods
  (syntax-parser
    #:datum-literals (on ->)
    [(_ ((on (event-name . _) . _) ...) target)
     #:with (unique-event ...)
     (remove-duplicates (map syntax-e (attribute event-name)))
     #'(begin
         (define/public (unique-event . args)
           (send/apply target unique-event args))
         ...)]))

(define-syntax compile-event-method
  (syntax-parser
    #:datum-literals (on ->)
    [(_ (on (event-name arg ...)
          action ...
          (-> name))
        machine)
     #'(define/public (event-name arg ...)
         action ...
         (send machine set-state name))]))

(machine
 #:initial-state idle
 (state idle
        (on-enter (displayln "pay a dollar"))
        (on (dollar)
            (-> paid))
        (on (select-item item)
            (displayln "you need to pay before selecting an item")
            (-> idle)))
 (state paid
        (on-enter (displayln "select an item"))
        (on (select-item item)
            (displayln (format "dispensing ~a" item))
            (-> idle))))
