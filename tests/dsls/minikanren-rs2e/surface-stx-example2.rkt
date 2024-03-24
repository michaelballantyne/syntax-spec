#lang racket/base

(require "surface-stx-example.rkt")

#;(run 1 (q)
  (conj
   (conj2
    (fresh1 (x)
            (== 1 1))
    (== 1 1))))

#;(run 1 (q)
  (conj
   (conj2
    (fresh (x)
            (== 1 1))
    (== 1 1))))

;; The syntax-original hack breaks when we generate the syntax...
(define-syntax-rule (m)
  (run 1 (q)
    (conj
     (conj2
      (fresh (x)
        (== 1 1))
      (== 1 1)))))
#;(m)

#;(run 1 (q)
    (fresh (z)
      (conj2
       (fresh (x)
         (== 1 1))
       (== 1 1))))

(defrel (r y)
  (fresh (z)
    (fresh (x)
      (== 1 1))
    (== 1 1)))

#;(run 1 (q)
    (fresh (z)
      (fresh (x)
        (== 1 1))
      (== 1 1)))

