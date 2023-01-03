#lang racket

(provide compile-machine)

(require syntax/parse/define (for-syntax syntax/parse))

(define-syntax compile-machine
  (syntax-parser
    #:datum-literals (machine state)
    [(_ initial-state (~and s (state . _)) ... e ...)
     #'(let ([st #f])
         (define common-handler
           (lambda (event)
             (compile-events e ... event [_ (error 'machine "no event handler matched for ~a" event)] st)))

         (compile-state s common-handler st)
         ...
         
         (set! st (initial-state))
         (lambda (event)
           (st event)))]))

(define-syntax compile-state
  (syntax-parser
    #:datum-literals (state on-enter)
    [(_ (state name
          (on-enter action ...)
          e ...)
        common-handler st) 
     #'(define (name)
         action ...
         (lambda (event)
           (compile-events e ... event [_ (common-handler event)] st)))]))

(define-syntax compile-events
  (syntax-parser
    #:datum-literals (on ->)
    [(_ (on (event-name arg ...)
         action ...
          (-> name))
        ...
        event else-clause st)
     #'(match event
         [`(event-name ,arg ...)
          action ...
          (set! st (name))]
         ...
         else-clause)]))