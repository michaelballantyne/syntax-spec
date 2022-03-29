#lang racket

(define-statechart reddit-browser
  (data subreddit #f)
  (data posts
  
  (state idle)
  (state selected
    (state loading
      (invoke fetch-subreddit
        (on (done val)
          (set posts val)
          (-> loaded))
        (on error (-> failed))))
    (state loaded)
    (state failed))

  (on (select val)
    (set subreddit val)
    (-> selected)))