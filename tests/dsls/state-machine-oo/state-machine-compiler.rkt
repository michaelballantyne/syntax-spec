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
        ...
        common-e ...)
     #:with (all-events ...) (unique-event-names #'(e ... ... common-e ...))
     #'(let ()
         (define machine%
           (class object%
             (define state #f)
             (define/public (set-state state%)
               (set! state (new state% [machine this])))

             (compile-proxy-method all-events state)
             ...

             (send this set-state initial-state)
             (super-new)))

         (define common%
           (class object%
             (init-field machine)

             (compile-event-method common-e machine)
             ...

             (super-new)))

         (define state-name
           (class common%
             (inherit-field machine)
             
             action
             ...

             (compile-event-method e machine)
             ...
             
             (super-new)))
         ...

         (new machine%))]))

(begin-for-syntax
  (define (unique-event-names evt-stxs)
    (remove-duplicates (map event-name (syntax->list evt-stxs))
                       bound-identifier=?))
  
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
    [(_ (on (event-name arg ...)
          action ...
          (-> name))
        machine)
     #'(define/public (event-name arg ...)
         action ...
         (send machine set-state name))]))