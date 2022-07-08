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

(module rt racket/base
  (require racket/match)

  (provide (all-defined-out))
  
  (struct machine [state step-f])

  (define (machine-step m evt)
    (match-define (machine state step-f) m)
    (define state^ (step-f state evt))
    (when (not state^)
      (event-error m evt))
    (machine state^ step-f))

  (define (state-error state)
    (error 'machine "invalid state: ~a" state))

  (define (event-error state event)
    (error 'machine "no transition for event ~a from state ~a" event (machine-state-list state)))

  (define (try-nested state event event-handler)
    (define outer-state (car state))
    (match-define (machine nested-state nested-step-f) (cadr state))
    
    (let ([nested-res (nested-step-f nested-state event)])
      (if nested-res
          (list outer-state (machine nested-res nested-step-f))
          (event-handler event))))

  (define (machine-state-list m)
    (match (machine-state m)
      [(list state-name)
       (list state-name)]
      [(list state-name nested)
       (cons state-name (machine-state-list nested))]))
  )

(require (prefix-in rt: 'rt))

(define machine-step rt:machine-step)
(define machine-state rt:machine-state-list)


;; Compiler

(begin-for-syntax
  (define-persistent-free-id-table compiled-machine-names)
  
  (define (compile-machine machine-spec)
    (syntax-parse machine-spec
      #:literals (state)
      [[#:initial initial-state:id (state state-name:id . state-body) ...]
       (with-syntax ([(compiled-body ...) (for/list ([sb (attribute state-body)])
                                            (compile-state-body #'state #'event-id sb))])
         #'(rt:machine
            'initial-state
            (lambda (state event-id)
              (match state
                ['state-name
                 compiled-body]
                ...
                [_ (rt:state-error state)]))))]))

  (define (compile-state-body state-arg event-id-arg body)
    (syntax-parse body
      #:literals (on ->)
      [((on (event-name:id) (-> next-state:id)) ...)
       (with-syntax ([state state-arg]
                     [event-id event-id-arg])
         #`(match event-id
             ['event-name 'next-state]
             ...
             [_ (rt:event-error state event-id)]))])))
          

(module+ test
  (require rackunit)

  (define walk-signal/manually-compiled
    (lambda ()
      (define (walk) '(walk))
      (define (caution) '(caution))
      (define (stop) '(stop))
      
      (define (step-f state event)
        (match (car state)
          ['walk
           (match event
             ['ped-time (caution)]
             [_ #f])]
          ['caution
           (match event
             ['ped-time (stop)]
             [_ #f])]
          ['stop
           (match event
             [_ #f])]))
      (rt:machine
       (walk)
       step-f)))
  
  (define traffic-light/manually-compiled
    ((lambda ()
       (define (green) '(green))
       (define (yellow) '(yellow))
       (define (red) (list 'red (walk-signal/manually-compiled)))
      
       (define (step-f state event)
         (match (car state)
           ['green
            (match event
              ['time (yellow)]
              [_ #f])]
           ['yellow
            (match event
              ['time (red)]
              [_ #f])]
           ['red
            (rt:try-nested state
                           event
                           (lambda (event)
                             (match event
                               ['time (green)]
                               [_ #f])))]))
       (rt:machine
        (green)
        step-f))))

  (define (test-traffic-light init-m)
    (define yellow-m (machine-step init-m 'time))
    
    (check-equal?
     (machine-state yellow-m)
     '(yellow))

    (define red-m (machine-step yellow-m 'time))

    (check-equal?
     (machine-state red-m)
     '(red walk))

    (define red-caution-m (machine-step red-m 'ped-time))

    (check-equal?
     (machine-state red-caution-m)
     '(red caution))
    
    (check-exn
     #rx"no transition for event foo from state \\(green\\)"
     (lambda ()
       (machine-step init-m 'foo)))

    (check-exn
     #rx"no transition for event foo from state \\(red caution\\)"
     (lambda ()
       (machine-step red-caution-m 'foo))))

  (test-traffic-light traffic-light/manually-compiled)
  
  #;(begin
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

  
      (test-traffic-light traffic-light))
  
  )


