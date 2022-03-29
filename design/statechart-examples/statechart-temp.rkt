#lang racket

(define-statechart temp
  (data C #f)
  (data F #f)
  
  (state active
    (on (celcius val)
      (set C val)
      (set F (+ (* val (/ 9 5)) 32))
    (on (farenheit val)
      (set C (* (- val 32) (/ 5 9)))
      (set F val)))))