#lang racket/base

(provide (all-defined-out))
(require "../../testing.rkt"
         racket/class
         (for-syntax racket/pretty racket/list))

(syntax-spec
  (binding-class event-var)
  (binding-class state-name)
  (extension-class state-macro)

  (nonterminal/exporting state-spec
    #:allow-extension state-macro

    (state name:state-name ((~datum on-enter) body:racket-expr ...+) transition:transition-spec ...)
    #:binding (export name)

    (state name:state-name transition:transition-spec ...)
    #:binding (export name))

  (nonterminal transition-spec
    (on (event-name:id arg:event-var ...)
        body:racket-expr
        ...
        ((~datum goto) next-state-name:state-name))
    #:binding (scope (bind arg) ... body ...))

  (host-interface/expression
    (machine #:initial initial-state:state-name s:state-spec ...)
    #:binding (scope (import s ...) initial-state)

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
    (define accessible-states (local-symbol-set))
    (define (find-state-spec state-name)
      (findf (lambda (state-spec)
               (compiled-identifier=? state-name (state-spec-name state-spec)))
             state-specs))
    (define (add-reachable-states! state-name)
      (unless (symbol-set-member? accessible-states state-name)
        (symbol-set-add! accessible-states state-name)
        (define state-spec (find-state-spec state-name))
        (for ([next-state-name (state-spec-next-state-names state-spec)])
          (add-reachable-states! next-state-name))))
    (add-reachable-states! initial-state-id)
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
         (~or ((~datum on-enter) . _)
              ((~datum on) ev
                  body
                  ...
                  (goto next-state-name)))
         ...)
       (attribute next-state-name)])))

(define-syntax compile-machine
  (syntax-parser
    #:datum-literals (machine state on-enter)
    [(_ initial-state
        (state state-name
          (~optional (on-enter action ...) #:defaults ([(action 1) '()]))
          e ...)
        ...)
     #'(with-reference-compilers ([event-var mutable-reference-compiler])
         (let ()
           (define machine%
             (class object%
               (define state #f)
               (define/public (set-state state%)
                 (set! state (new state% [machine this])))
               (define/public (get-state)
                 (send state get-state))

               (compile-proxy-methods (e ... ...) state)

               (send this set-state initial-state)
               (super-new)))

           (define state-name
             (class object%
               (init-field machine)
               (define/public (get-state)
                 'state-name)
               action ...
               (compile-event-method e machine) ...
               (super-new)))
           ...

           (new machine%)))]))

(define-syntax compile-proxy-methods
  (syntax-parser
    #:datum-literals (on goto)
    [(_ ((on (event-name . _) . _) ...) target)
     #:with (unique-event ...)
     (remove-duplicates (map syntax-e (attribute event-name)))
     #'(begin
         (define/public (unique-event . args)
           (send/apply target unique-event args))
         ...)]))

(define-syntax compile-event-method
  (syntax-parser
    #:datum-literals (on goto)
    [(_ (on (event-name arg ...)
          action ...
          (goto name))
        machine)
     #'(define/public (event-name arg ...)
         action ...
         (send machine set-state name))]))

(define-syntax-rule
  (define-state-syntax name trans)
  (define-dsl-syntax name state-macro trans))

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
                'red)
  (let ()
    (define x 0)
    (define y 0)
    (define z 0)
    (define mchn-with-actions
      (machine
       #:initial start
       (state start
              (on-enter (set! x 1))
              (on (ev)
                  (set! y 1)
                  (goto end)))
       (state end
              (on-enter (set! z 1)))))
    (check-equal? (list x y z) '(1 0 0))
    (send mchn-with-actions ev)
    (check-equal? (list x y z) '(1 1 1))))
