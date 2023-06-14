#lang racket/base

(require "mk.rkt" "facts.rkt")

(define-facts-table flights [flightfrom flightto])


(assert-fact flights "lax" "slc")
(assert-fact flights "bos" "slc")
(assert-fact flights "bos" "lax")

(run* (q)
  (fresh (from)
    (== from "bos")
    (query-facts flights from q)))