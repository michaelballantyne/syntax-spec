#lang racket

(require "../main.rkt"
         (only-in "../private/forms.rkt" plain-alt))

(define-peg p1
  (plain-alt "==" (plain-alt ">=" (plain-alt "<=" (plain-alt "<" (plain-alt ">" (plain-alt "!=" (plain-alt "in" (seq "not" " " "in")))))))))

(define-peg p2
  (alt "==" ">=" "<=" "<" ">" "!=" "in" (seq "not" " " "in")))

(define times 10000000)

(time
 (for ([n (in-range times)])
   (parse p1 "in")))

(time
 (for ([n (in-range times)])
   (parse p2 "in")))
