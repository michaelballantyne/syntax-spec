#lang racket

(define-statechart timer
  (data elapsed 0)
  (data duration 5)
  (data interval 0.1)

  (state running
    (invoke
     (lambda (cb)
       (define i (set-interval (lambda () (cb tick))))
       (lambda ()
         (clear-interval i))))

    (on eps
      (when elapsed >= duration)
      (-> paused))
    
    (on (tick)
      (action
        (set elapsed (+ elapsed interval)))))

  (state paused
    (on eps
      (cond (< elapsed duration))
      (-> running)))

  (on (duration.update val)
    (set duration val))

  (on (reset)
    (set elapsed 0)))