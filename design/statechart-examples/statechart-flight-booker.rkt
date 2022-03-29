#lang racket

(define-statechart flight-kind
  (data start-date #f)
  (data return-date #f)
  (data trip 'one-way)

  (state editing
    (on (start-widget.update val)
       (set start-date val))
    (on (return-widget.update val)
       (set return-date val))
    (on (set-trip val)
       (set trip val))

    (on (submit)
      (when (if (eq? trip 'one-way)
                start-date
                (and start-date
                   return-date
                   (date> return-date start-date))))
      (-> submitted)))
  
  (state submitted
     (final)))
