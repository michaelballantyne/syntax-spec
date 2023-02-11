#lang racket

(require
  "../core.rkt")

(define-peg t1
  (=> (seq "a" (seq (: r "b") "c"))
      5))

(define-peg t2
  (=> (* (: r "a"))
      r))

(define-peg t3
  (=> (* (seq "a" (* (: r "b"))))
      r))

(define-peg t4
  (=> (alt (: a "a") (: b "b"))
      (list a b)))

(define-peg t5
  (=> (* (seq (seq (! "b") (: c (char (lambda (c) #t)))) eps))
      c))

(define-peg t6
  (alt (=> "b" '())
       (=> (seq (: a "a") (: d t6))
           (cons a d))))

(define-peg t7
  (=> (: r (seq "ab" (seq "c" (* (seq (! "f") (char (lambda (c) #t)))))))
      r))

(module+ test
  (require rackunit)
  
  (check-equal?
   (parse-result-value (parse t1 "abcd"))
   5)

  (check-equal?
   (parse-result-value (parse t2 "aaa"))
   '("a" "a" "a"))

  (check-equal?
   (parse-result-value (parse t3 "ababbabbb"))
   '(("b") ("b" "b") ("b" "b" "b")))

  (check-equal?
   (parse-result-value (parse t4 "b"))
   '(#f "b"))

  (check-equal?
   (parse-result-value (parse t5 "aaab"))
   '("a" "a" "a"))

  (check-equal?
   (parse-result-value (parse t6 "aaab"))
   '("a" "a" "a"))

  (check-equal?
   (parse-result-value (parse t7 "abcddf"))
   "abcdd")
  
  )
