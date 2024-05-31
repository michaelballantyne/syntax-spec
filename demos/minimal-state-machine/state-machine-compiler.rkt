#lang racket

(provide compile-machine)

(require syntax/parse/define (for-syntax syntax/parse racket/list))

(define-syntax compile-machine
  (syntax-parser
    #:datum-literals (machine state on-enter)
    [(_ initial-state
        (state state-name
          (on-enter action ...)
          e ...)
        ...)
     #'(let ()
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

         (new machine%))]))

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