#lang racket/base

(require racket/class racket/match)

(define state-interface (interface () describe-state step))

(define simple-state%
  (class* object% (state-interface)
    (super-new)
    
    (init-field name)
    (init-field handler)
    
    (define/public (describe-state)
      (list name))
    (define/public (step event)
      (handler event))))

(define state-with-nested%
  (class* object% (state-interface)
    (super-new)
    
    (init-field name)
    (init-field handler)
    (init-field nested)

    (define/public (describe-state)
      (cons name (send nested describe-state)))

    (define/public (step event)
      (or (send nested step event)
          (handler event)))))


(define walk-signal/manually-compiled
  (lambda ()
    (define (walk) (new simple-state%
                        [name 'walk]
                        [handler (lambda (event)
                                   (match event
                                     ['ped-time (caution)]
                                     [_ #f]))]))
    (define (caution) (new simple-state%
                           [name 'caution]
                           [handler (lambda (event)
                                      (match event
                                        ['ped-time (stop)]
                                        [_ #f]))]))
    (define (stop) (new simple-state%
                        [name 'stop]
                        [handler (lambda (event)
                                   (match event
                                     [_ #f]))]))
    (walk)))

(define traffic-light/manually-compiled
  ((lambda ()
     (define (green) (new simple-state%
                          [name 'green]
                          [handler (lambda (event)
                                     (match event
                                       ['time (yellow)]
                                       [_ #f]))]))
     (define (yellow) (new simple-state%
                           [name 'yellow]
                           [handler (lambda (event)
                                      (match event
                                        ['time (red)]
                                        [_ #f]))]))
     (define (red) (new state-with-nested%
                        [name 'red]
                        [nested (walk-signal/manually-compiled)]
                        [handler (lambda (event)
                                   (match event
                                     ['time (green)]
                                     [_ #f]))]))
     (green))))

(define (machine-step m event)
  (let ([res (send m step event)])
    (or res
        (error 'machine-step "no transition for event ~a from state ~a" event (send m describe-state)))))

(define (machine-state m)
  (send m describe-state))

(module+ test
  (require rackunit)
  (define (test-traffic-light init-m)
    (define yellow-m (machine-step init-m 'time))
    
    (check-equal?
     (machine-state yellow-m)
     '(yellow))

    (define red-m (machine-step yellow-m 'time))

    (check-equal?
     (machine-state red-m)
     '(red walk))
    
    (check-exn
     #rx"no transition for event foo from state \\(green\\)"
     (lambda ()
       (machine-step init-m 'foo))))

  (test-traffic-light traffic-light/manually-compiled))


