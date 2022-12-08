#lang racket

(require "statecharts.rkt")

(define (turnstile fare)
  (machine
   #:initial locked

   (state unlocked
     (on (pass)
       (emit 'lock)
       (-> locked)))
       
   (data accumulated-value 0)
     
   (state locked
     (on (coin value1) #:when (>= value1 (- fare accumulated-value))
       (set accumulated-value 0)
       (emit 'unlock)
       (-> unlocked))
     (on (coin value2)
       (let* ([new-value accumulated-value]
              [new-value (+ value2 new-value)])
         (set accumulated-value new-value))
       (-> locked)))

   ))

(machine-step
 (turnstile 5)
 '(coin 4))