#lang racket/base

(require bindingspec
         racket/match
         
         (for-syntax racket/base
                     syntax/parse))

;; Syntax

(define-hosted-syntaxes
  (binding-class data-var)
  (binding-class local-var)
  (binding-class machine-name)
  (binding-class state-name)

  (nonterminal machine-spec
    [#:initial initial-state:state-name s:machine-element-spec ...]
    #:binding {(recursive s) initial-state})
  
  (two-pass-nonterminal machine-element-spec
    (data v:data-var e:expr)
    #:binding (export v)

    (state n:state-name
      e:event-spec ...)
    #:binding (export n)
    
    (composite-state n:state-name
      #:machine nm:machine-name
      e:event-spec ...)
    #:binding (export n))

  (nonterminal event-spec
    (~> ((~literal on) header body:expr ...)
        #'(on header #:when (lambda (v) #t) body ...))
    
    (on (evt:id arg:local-var ...) #:when guard:expr
      . b:event-body)
    #:binding {(bind arg) (host guard) b})

  (nonterminal event-body
    [((~literal let*) (b:binding ...) . body:event-body)]
    #:binding (nest b body)
    
    [a:action-spec ...
     t:transition-spec])

  (nesting-nonterminal binding (nested)    
    [v:local-var e:expr]
    #:binding [(host e) {(bind v) nested}])
  
  (nonterminal action-spec
    (set v:data-var e:expr)
    #:binding (host e))

  (nonterminal transition-spec
    (-> s:state-name)))

;; Interface

(define-host-interface/expression
  (machine . spec:machine-spec)
  #:with machine-constructor (compile-machine #'spec)
  #'(machine-constructor))

(define-host-interface/definitions
  (define-machine n:machine-name . spec:machine-spec)
  #:binding (export n)
  #:with compiled-n (compile-binder! compiled-ids #'n)
  #:with machine-constructor (compile-machine #'spec)
  #'(define compiled-n machine-constructor))
  

;; Runtime

(module rt racket/base
  (require racket/match)

  (provide (all-defined-out))
  
  (struct machine [state data step-f])

  (define (machine-step m evt)
    (match-define (machine state data step-f) m)
    
    (define-values (state^ data^) (step-f state data evt))
    (when (not state^)
      (event-error m evt))
    (machine state^ data^ step-f))

  (define (state-error state)
    (error 'machine "invalid state: ~a" state))

  (define (event-error state event)
    (error 'machine
           "no transition for event ~a from state ~a"
           event (machine-state-list state)))

  (define (try-nested state data event event-handler)
    (define outer-state (car state))
    (match-define (machine nested-state nested-data nested-step-f) (cadr state))
    
    (let-values ([(nested-state^ nested-data^) (nested-step-f nested-state nested-data event)])
      (if nested-state^
          (values (list outer-state (machine nested-state^ nested-data^ nested-step-f)) data)
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
  (define-persistent-free-id-table compiled-ids)

  (define-syntax-class (compile-state/cls state-id data-id event-id)
    #:literals (composite-state state)
    (pattern
      (state state-name:id
        . events)
      
      #:attr constructor
      #'(define (state-name) '(state-name))
      
      #:attr step
      (compile-dispatch event-id data-id #'events))
    
    (pattern
      (composite-state state-name:id
        #:machine nested:id
        . events)

      #:attr constructor
      (with-syntax ([compiled-nested (compile-reference compiled-ids #'nested)])
        #'(define (state-name) (list 'state-name (compiled-nested))))
      
      #:attr step
      #`(rt:try-nested #,state-id
                       #,data-id
                       #,event-id
                       (lambda (event)
                         #,(compile-dispatch #'event data-id #'events)))))
      

  (define (compile-dispatch event-id data-id events)
    (define/syntax-parse (compiled-clause ...)
      (for/list ([event (syntax->list events)])
        (compile-dispatch-clause data-id event)))
    #`(match #,event-id
        compiled-clause
        ...
        [_ (values #f #f)]))

  (define (compile-host-expression stx)
    (define (compile-simple-ref id)
      (compile-reference compiled-ids id))
    
    (resume-host-expansion
     stx
     #:reference-compilers
     ([data-var compile-simple-ref]
      [local-var compile-simple-ref])))


  (define (compile-event-body data-id event-body)
    (syntax-parse event-body
      [[(let* ([v:id e] ...) . body)]
       #:with (v-c ...) (compile-binders! compiled-ids #'(v ...))
       #:with (e-c ...) (map compile-host-expression (attribute e))
       #`(let* ([v-c e-c] ...)
           #,(compile-event-body data-id #'body))]
      [[(set data-var:id rhs:expr)
        ...
        (-> next-state)]
       #:with (rhs-c ...) (map compile-host-expression (attribute rhs))
       #`(values (next-state)
                 (struct-copy machine-data #,data-id
                              [data-var rhs-c]
                              ...))]))
  
  (define (compile-dispatch-clause data-id event)
    (syntax-parse event
      #:literals (let* on ->)
      [(on (event-name:id arg:id ...)
         #:when guard:expr
         . event-body)
       #:with (arg-c ...) (compile-binders! compiled-ids #'(arg ...))
       #:with guard-c (compile-host-expression #'guard)
       #`[(list 'event-name arg-c ...)
          #:when guard-c
          #,(compile-event-body data-id #'event-body)]]))
  
  (define (compile-machine machine-spec)
    (syntax-parse machine-spec
      #:literals (data state on ->)
      [[#:initial initial-state:id
        (data data-var:id data-rhs:expr)
        ...
        (~var st (compile-state/cls #'state #'data #'event))
        ...]
       #:with (compiled-data-var ...) (compile-binders! compiled-ids #'(data-var ...))
       #'(lambda ()
           (struct machine-data [data-var ...] #:prefab)
           
           st.constructor
           ...

           (define (step-f state data event)
             (match-define (machine-data compiled-data-var ...) data)
             (match (car state)
               ['st.state-name
                st.step]
               ...))
             
           (rt:machine
            (initial-state)
            (machine-data data-rhs ...)
            step-f))])))


(module+ test
  (require rackunit)

  ;;
  ;; Traffic light example
  ;;

  (define walk-signal/manually-compiled
    (lambda ()
      (define (walk) '(walk))
      (define (caution) '(caution))
      (define (stop) '(stop))
      
      (define (step-f state data event)
        (match (car state)
          ['walk
           (match event
             ['(ped-time) (values (caution) data)]
             [_ (values #f #f)])]
          ['caution
           (match event
             ['(ped-time) (values (stop) data)]
             [_ (values #f #f)])]
          ['stop
           (match event
             [_ (values #f #f)])]))
      (rt:machine
       (walk)
       (list)
       step-f)))
  
  (define traffic-light/manually-compiled
    ((lambda ()
       (define (green) '(green))
       (define (yellow) '(yellow))
       (define (red) (list 'red (walk-signal/manually-compiled)))
      
       (define (step-f state data event)
         (match (car state)
           ['green
            (match event
              ['(time) (values (yellow) data)]
              [_ (values #f #f)])]
           ['yellow
            (match event
              ['(time) (values (red) data)]
              [_ (values #f #f)])]
           ['red
            (rt:try-nested state
                           data
                           event
                           (lambda (event)
                             (match event
                               ['(time) (values (green) data)]
                               [_ (values #f #f)])))]))
       (rt:machine
        (green)
        (list)
        step-f))))

  (define (test-traffic-light init-m)
    (define yellow-m (machine-step init-m '(time)))
    
    (check-equal?
     (machine-state yellow-m)
     '(yellow))


    (define red-m (machine-step yellow-m '(time)))

    (check-equal?
     (machine-state red-m)
     '(red walk))

    (define red-caution-m (machine-step red-m '(ped-time)))

    (check-equal?
     (machine-state red-caution-m)
     '(red caution))
    
    (check-exn
     #rx"no transition for event \\(foo\\) from state \\(green\\)"
     (lambda ()
       (machine-step init-m '(foo))))

    (check-exn
     #rx"no transition for event \\(foo\\) from state \\(red caution\\)"
     (lambda ()
       (machine-step red-caution-m '(foo))))
    )

  (test-traffic-light traffic-light/manually-compiled)

  (begin
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
       (composite-state red
         #:machine walk-signal
         (on (time) (-> green)))))
  
    (test-traffic-light traffic-light))

  
  ;;
  ;; Turnstile example
  ;;

  (define (turnstile/manually-compiled fare)
    ((lambda ()
       (struct machine-data [accumulated-value] #:prefab)
       
       (define (locked) '(locked))
       (define (unlocked) '(unlocked))
      
       (define (step-f state data event)
         (match-define (machine-data accumulated-value) data)
         (match (car state)
           ['locked
            (match event
              [(list 'coin value)
               #:when (>= value (- fare accumulated-value))
               (values (unlocked) (struct-copy machine-data data
                                               [accumulated-value 0]))]
              [(list 'coin value)
               (values (locked) (struct-copy machine-data data
                                             [accumulated-value (+ value accumulated-value)]))]
              [_ (values #f #f)])]
           ['unlocked
            (match event
              ['(pass) (values (locked) data)]
              [_ (values #f #f)])]))
       (rt:machine
        (locked)
        (machine-data 0)
        step-f))))

  (define (test-turnstile turnstile)
    (define locked-m (turnstile 5))

    (check-equal?
     (machine-state locked-m)
     '(locked))

    (define still-locked-m (machine-step locked-m '(coin 4)))
    
    (check-equal?
     (machine-state still-locked-m)
     '(locked))

    (define unlocked1 (machine-step locked-m '(coin 5)))
    (check-equal?
     (machine-state unlocked1)
     '(unlocked))
    
    (define unlocked2 (machine-step still-locked-m '(coin 1)))
    (check-equal?
     (machine-state unlocked2)
     '(unlocked))
    )

  (test-turnstile turnstile/manually-compiled)

  (begin
    (define (turnstile fare)
      (machine
       #:initial locked

       (data accumulated-value 0)
     
       (state locked
         (on (coin value) #:when (>= value (- fare accumulated-value))
           (set accumulated-value 0)
           (-> unlocked))
         (on (coin value)
           (let* ([new-value accumulated-value]
                  [new-value (+ value new-value)])
             (set accumulated-value new-value)
             (-> locked))))
     
       (state unlocked
         (on (pass) (-> locked)))))
 
    (test-turnstile turnstile))
  )

(require macro-debugger/stepper)

