#lang racket/base

(provide (all-defined-out))
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

    (state n:state-name #:nested-machine nm:machine-name e:event-spec ...)
    #:binding [(export n) nm e]
    
    (state n:state-name e:event-spec ...)
    #:binding [(export n) e])

  (nonterminal event-spec
    (on (evt:id arg:local-var ...) #:when guard:expr b:action-spec ... t:transition-spec)
    #:binding {(bind arg) (host guard) b}
    
    (on (evt:id arg:local-var ...) b:action-spec ... t:transition-spec)
    #:binding {(bind arg) b})

  (nesting-nonterminal binding (nested)    
    [v:local-var e:expr]
    #:binding [(host e) {(bind v) nested}])
  
  (nonterminal action-spec
    ((~literal let*) (b:binding ...) a:action-spec ...)
    #:binding (nest b a)
    
    (set v:data-var e:expr)
    #:binding (host e)

    (emit e:expr)
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
  #:with machine-constructor (compile-machine #'spec)
  #'(define n machine-constructor))
  

;; Runtime

(module rt racket/base
  (require racket/match)

  (provide (all-defined-out))
  
  (struct machine [state data step-f])

  (define (machine-step m evt)
    (match-define (machine state data step-f) m)
    
    (define-values (state^ data^ reactions) (step-f state data evt))
    (when (not state^)
      (event-error m evt))
    (values (machine state^ data^ step-f) reactions))

  (define (no-transition)
    (values #f #f #f))

  (define (state-error state)
    (error 'machine "invalid state: ~a" state))

  (define (event-error state event)
    (error 'machine
           "no transition for event ~a from state ~a"
           event (machine-state-list state)))

  (define (try-nested state data event event-handler)
    (define outer-state (car state))
    (match-define (machine nested-state nested-data nested-step-f) (cadr state))
    
    (let-values ([(nested-state^ nested-data^ reactions) (nested-step-f nested-state nested-data event)])
      (if nested-state^
          (values (list outer-state (machine nested-state^ nested-data^ nested-step-f)) data reactions)
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
  (define-syntax-class (compile-state/cls state-id data-id event-id)
    #:literals (state)
    (pattern
     (state state-name:id
            #:nested-machine nested:id
            . events)

     #:attr constructor
     #'(define (state-name) (list 'state-name (nested)))

     #:attr step
     #`(rt:try-nested #,state-id
                      #,data-id
                      #,event-id
                      (lambda (event)
                        #,(compile-dispatch #'event data-id #'events))))
    (pattern
      (state state-name:id
        . events)
      
      #:attr constructor
      #'(define (state-name) '(state-name))
      
      #:attr step
      (compile-dispatch event-id data-id #'events)))
      

  (define (compile-dispatch event-id data-id events)
    (define/syntax-parse (compiled-clause ...)
      (for/list ([event (syntax->list events)])
        (compile-dispatch-clause data-id event)))
    #`(match #,event-id
        compiled-clause
        ...
        [_ (rt:no-transition)]))

  (define (action-expr-bindings action-expr)
    (syntax-parse action-expr
      #:literals (let*)
      [(let* (binding ...) body ...)
       (apply append
              (attribute binding)
              (map action-expr-bindings (attribute body)))]
      [_ '()]))

  (define (action-expr-actions action-expr)
    (syntax-parse action-expr
      #:literals (let*)
      [(let* (binding ...) body ...)
       (apply append (map action-expr-actions (attribute body)))]
      [_ (list this-syntax)]))

  (define (compile-event-body data-id event-body)
    (syntax-parse event-body
      #:literals (let* set emit ->)
      [[action ... (-> next-state)]
       #:with ([v e] ...) (apply append (map action-expr-bindings (attribute action)))
       #:with ((~alt (set data-var:id rhs:expr)
                     (emit emit-e:expr))
               ...)
       (apply append (map action-expr-actions (attribute action)))
       
       #`(let* ([v e] ...)
           (values (next-state)
                   (struct-copy machine-data #,data-id
                                [data-var rhs]
                                ...)
                   (list emit-e ...)))]))
  
  (define (compile-dispatch-clause data-id event)
    (syntax-parse event
      #:literals (on)
      [(on (event-name:id arg:id ...)
         (~optional (~seq #:when guard:expr))
         . event-body)
       #`[(list 'event-name arg ...)
          (~@ . (~? (#:when guard) ()))
          #,(compile-event-body data-id #'event-body)]]))
  
  (define (compile-machine machine-spec)
    (syntax-parse machine-spec
      #:literals (data state on ->)
      [[#:initial initial-state:id
        (~alt (data data-v:id data-rhs:expr)
              (~var st (compile-state/cls #'state #'data #'event)))
        ...]
       #'(lambda ()
           (with-reference-compilers ([data-var immutable-reference-compiler]
                                      [local-var immutable-reference-compiler])
        
             (struct machine-data [data-v ...] #:prefab)
           
             st.constructor
             ...

             (define (step-f state data event)
               (match-define (machine-data data-v ...) data)
               (match (car state)
                 ['st.state-name
                  st.step]
                 ...))
             
             (rt:machine
              (initial-state)
              (machine-data data-rhs ...)
              step-f)))])))


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
             ['(ped-time) (values (caution) data '())]
             [_ (rt:no-transition)])]
          ['caution
           (match event
             ['(ped-time) (values (stop) data '())]
             [_ (rt:no-transition)])]
          ['stop
           (match event
             [_ (rt:no-transition)])]))
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
              ['(time) (values (yellow) data '())]
              [_ (rt:no-transition)])]
           ['yellow
            (match event
              ['(time) (values (red) data '())]
              [_ (rt:no-transition)])]
           ['red
            (rt:try-nested state
                           data
                           event
                           (lambda (event)
                             (match event
                               ['(time) (values (green) data '())]
                               [_ (rt:no-transition)])))]))
       (rt:machine
        (green)
        (list)
        step-f))))

  (define (test-traffic-light init-m)
    (define-values (yellow-m _1) (machine-step init-m '(time)))
    
    (check-equal?
     (machine-state yellow-m)
     '(yellow))


    (define-values (red-m _2) (machine-step yellow-m '(time)))

    (check-equal?
     (machine-state red-m)
     '(red walk))

    (define-values (red-caution-m _3) (machine-step red-m '(ped-time)))

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
       (state red
         #:nested-machine walk-signal
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
               (values (unlocked)
                       (struct-copy machine-data data
                                    [accumulated-value 0])
                       '(unlock))]
              [(list 'coin value)
               (values (locked)
                       (struct-copy machine-data data
                                    [accumulated-value (+ value accumulated-value)])
                       '())]
              [_ (rt:no-transition)])]
           ['unlocked
            (match event
              ['(pass) (values (locked)
                               data
                               '(lock))]
              [_ (rt:no-transition)])]))
       (rt:machine
        (locked)
        (machine-data 0)
        step-f))))

  (define (test-turnstile turnstile)
    (define locked-m (turnstile 5))

    (check-equal?
     (machine-state locked-m)
     '(locked))

    (define-values (still-locked-m still-locked-e*) (machine-step locked-m '(coin 4)))
    
    (check-equal?
     (machine-state still-locked-m)
     '(locked))

    (define-values (unlocked1 unlocked1-e*) (machine-step locked-m '(coin 5)))
    (check-equal?
     (machine-state unlocked1)
     '(unlocked))
    (check-equal?
     unlocked1-e*
     '(unlock))
    
    (define-values (unlocked2 unlocked2-e*) (machine-step still-locked-m '(coin 1)))
    (check-equal?
     (machine-state unlocked2)
     '(unlocked))
    (check-equal?
     unlocked2-e*
     '(unlock))

    (define-values (locked-again-m locked-again-e*) (machine-step unlocked2 '(pass)))
    (check-equal?
     (machine-state locked-again-m)
     '(locked))
    (check-equal?
     locked-again-e*
     '(lock))  
    )

  (test-turnstile turnstile/manually-compiled)

  (begin
    (define (turnstile fare)
      (machine
       #:initial locked

       (state unlocked
         (on (pass)
           (emit 'lock)
           (-> locked)))
       
       (data accumulated-value 0)
     
       (state locked
         (on (coin value) #:when (>= value (- fare accumulated-value))
           (set accumulated-value 0)
           (emit 'unlock)
           (-> unlocked))
         (on (coin value)
           (let* ([new-value accumulated-value]
                  [new-value (+ value new-value)])
             (set accumulated-value new-value))
           (-> locked)))

       ))
 
    (test-turnstile turnstile))
  )

(require macro-debugger/stepper)

