#lang racket/base

(require bindingspec
         syntax/parse/define
         racket/match
         
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax))

;; Syntax

(define-hosted-syntaxes
  (binding-class machine-name)
  (binding-class state-name)

  (nonterminal machine-spec
    [#:initial initial-state:state-name s:state-spec ...]
    #:binding {(recursive s) initial-state})
  
  (two-pass-nonterminal state-spec
    (state n:state-name
           e:event-spec ...)
    #:binding (export n))

  (nonterminal event-spec
    (on (evt:id)
        ab:action))

  (nonterminal action
    (-> s:state-name)))

;; Interface

(define-host-interface/expression
  (machine . spec:machine-spec)
  #:binding []
  (compile-machine #'spec))


(define-host-interface/definitions
  (define-machine n:machine-name . spec:machine-spec)
  #:binding (export n)
  #:with compiled-n (compile-binder! compiled-machine-names #'n)
  #:with compiled-rhs (compile-machine #'spec)
  #'(define compiled-n compiled-rhs))
  

;; Runtime

(struct machine-state
  [state-id step-f])

(define (machine-step m evt)
  (match-define (machine-state state-id step-f) m)
  (define state-id^ (step-f state-id evt))
  (machine-state state-id^ step-f))

(define (state-error state-id)
  (error 'machine "invalid state: ~a" state-id))

(define (event-error state-id event-id)
  (error 'machine "no transition for event ~a from state ~a" event-id state-id))

;; Compiler

(begin-for-syntax
  (define-persistent-free-id-table compiled-machine-names)
  
  (define (compile-machine machine-spec)
    (syntax-parse machine-spec
      #:literals (state)
      [[#:initial initial-state:id (state state-name:id . state-body) ...]
       (with-syntax ([(compiled-body ...) (for/list ([sb (attribute state-body)])
                                            (compile-state-body #'state-id #'event-id sb))])
         #'(machine-state
            'initial-state
            (lambda (state-id event-id)
              (match state-id
                ['state-name
                 compiled-body]
                ...
                [_ (state-error state-id)]))))]))

  (define (compile-state-body state-id-arg event-id-arg body)
    (syntax-parse body
      #:literals (on ->)
      [((on (event-name:id) (-> next-state:id)) ...)
       (with-syntax ([state-id state-id-arg]
                     [event-id event-id-arg])
         #`(match event-id
             ['event-name 'next-state]
             ...
             [_ (event-error state-id event-id)]))])))
          

(module+ test
  (require rackunit)

  (define walk-signal/manually-compiled
    (machine-state
     'walk
     (lambda (state-id event-id)
       (match state-id
         ['walk
          (match event-id
            ['ped-time 'caution]
            [_ (event-error state-id event-id)])]
         ['caution
          (match event-id
            ['ped-time 'stop]
            [_ (event-error state-id event-id)])]
         ['stop
          (match event-id
            [_ (event-error state-id event-id)])]
         [_ (state-error state-id)]))))
  
  (define traffic-light/manually-compiled
    (machine-state
     'green
     (lambda (state-id event-id)
       (match state-id
         ['green
          (match event-id
            ['time 'yellow]
            [_ (event-error state-id event-id)])]
         ['yellow
          (match event-id
            ['time 'red]
            [_ (event-error state-id event-id)])]
         ['red
          (match event-id
            ['time 'green]
            [_ (event-error state-id event-id)])]
         [_ (state-error state-id)]))))

  (define-machine walk-signal
    #:initial walk
    (state walk
           (on (ped-time) (-> caution)))
    (state caution
           (on (ped-time) (-> stop)))
    (state stop))
  
  (define traffic-light
    (machine
     #:initial green
     (state green
            (on (time) (-> yellow)))
     (state yellow
            (on (time) (-> red)))
     (state red
            (on (time) (-> green)))))

  (define (test-traffic-light init-m)
    (check-equal?
     (machine-state-state-id (machine-step init-m 'time))
     'yellow)

    (check-exn
     #rx"no transition for event foo from state green"
     (lambda ()
       (machine-step init-m 'foo))))

  (test-traffic-light traffic-light/manually-compiled)
  (test-traffic-light traffic-light))


