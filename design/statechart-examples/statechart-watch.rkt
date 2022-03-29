#lang racket

(define-statechart watch
  (state dislays
    (state time
      (on button-d (-> date))
      (on button-a (-> alarm1)))
    (state date
      (on button-d (-> time))
      (timeout (minutes 2) (-> time)))
    (state alarm1
      (init disabled #:history)
      (state disabled)
      (state enabled)
      (on button-a (-> alarm2)))
    (state alarm2
      (init disabled #:history)
      (state disabled)
      (state enabled)
      (on button-a (-> chime)))
    (state chime
      (on button-a (-> stopwatch)))
    (state stopwatch
      (on button-a (-> time))))
  
  (state alarms
    (state alarm1)
    (state alarm2)
    (state both-alarms)
         
    (on button
      (-> displays)))