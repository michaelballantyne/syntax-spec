#lang racket

(define-statechart machine
  (context count 0)

  (event increment)
   
  (state active
    (on increment
      (set count (+ count 1)))))


(send-event machine increment)