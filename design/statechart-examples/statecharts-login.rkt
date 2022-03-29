#lang racket

; Very much not complete...

(define-statechart submitready
  (state missing-input)
  (state ready))

(define-statechart authenticate
  (state connecting)
  (state connected))

(define-statechart session
  (data time)

  (state connected)
  (state disconnected))

(define-statechart login
  (parallel
   submitready
   authenticate))

(define-statechart login-with-freeze
  (data attempts 0)

  (state can-login
    login)
  (state frozen))
  
  

  