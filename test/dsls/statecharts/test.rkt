#lang racket
(require "statecharts.rkt")
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