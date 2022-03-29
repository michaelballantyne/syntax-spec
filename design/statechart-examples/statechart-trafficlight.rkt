#lang racket

(define-statechart walk-light
  (initial walk)
  (state walk
    (on walk-timer (-> countdown)))
  (state countdown
    (on countdown-timer (-> stop)))
  (state stop))

(define-statechart stop-light
  (state green
    (on green-timer (-> yellow))
    (use walk-light #:as ped))
  (state yellow
    (on yellow-timer (-> red)))
  (state red
    (on red-timer (-> green))))
  