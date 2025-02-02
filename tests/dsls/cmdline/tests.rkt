#lang racket/base

(require "cmdline.rkt" "sugar.rkt"
         rackunit
         (for-syntax racket/base syntax/parse syntax/stx))

; testing
(check-equal?
 (let ()
   (define/command-line-options/surface
     #:argv (list "-a" "A" "-b" "B")
     #:options
     [a (choice/o #:required ["-a" v "the a flag" v])]
     [b (choice/o #:required ["-b" v "the b flag" v])])
   (list a b))
 '("A" "B"))

; simple arguments
(let ()
  (define/command-line-options/surface
    #:argv (list "1" "2")
    #:arguments x y)
  (check-equal? x "1")
  (check-equal? y "2"))

; checked arguments
(let ()
  (define/command-line-options/surface
    #:argv (list "1")
    #:arguments
    [x (int-range/p 0 100)])
  (check-equal? x 1))

; checked rest arguments
(let ()
  (define/command-line-options/surface
    #:argv (list "1" "2" "3")
    #:arguments
    [initial-value (int-range/p 0 100)]
    #:rest
    [adds (int-range/p 0 100)])
  (check-equal? initial-value 1)
  (check-equal? adds '(2 3)))

(define-syntax test-option
  (syntax-parser
    [(_ option-stx case ...)
     (define (gen-case stx)
       (syntax-parse stx
         [[args-e expected-e]
          (syntax/loc this-syntax (check-equal? (f args-e) expected-e))]))
     #`(let ()
         (define (f args)
           (define/command-line-options/surface
             #:argv args
             #:options
             [opt option-stx])
           opt)
         #,@(stx-map gen-case #'(case ...)))]))

; choice, no argument
(test-option
 (choice/o #:default #f ["-o" "enable optimizations" #t])
 ['() #f] ['("-o") #t])

; multi, simple argument
(test-option
 (multi/o '() [["-l" "--lf"] lf "add link flag <lf>" (lambda (acc) (append acc (list lf)))])
 ['() '()]
 ['("--lf" "a" "-l" "b") '("a" "b")])

; multi, checked arguments
(test-option
 (multi/o '() ["--c" [x (int-range/p 0 100)]
                   [y (int-range/p 0 100)]
                   "add coordinate (<x>, <y>)"
                   (lambda (acc) (append acc (list (cons x y))))])
 ['() '()]
 ['("--c" "0" "0" "--c" "1" "2") '((0 . 0) (1 . 2))])

; option macros
(test-option
 (switch/o ["-v" "--verbose"] "enable verbose output")
 ['() #f] ['("--verbose") #t] ['("-v") #t])

(test-option
 (list/o [["--lf" "-l"] lf "add a link flag"])
 ['() '()]
 ['("--lf" "a" "-l" "b") '("a" "b")])

(test-option
 (choice/o #:default 0
         ["--optimize-level" [lvl (int-range/p 0 3)]
                             "set optimization level to <lvl>" lvl]
         (numbered-flags/f "--o" [0 3] "optimization level"))
 ['() 0] ['("--optimize-level" "1") 1]
 ['("--o1") 1] ['("--o2") 2])

; argument type syntaxes
(test-option
 (choice/o #:required ["--coord" [x (int-range/p 0 100)] [y (int-range/p 0 100)]
                               "set coordinate" (cons x y)])
 ['("--coord" "0" "1") '(0 . 1)])

; argument errors
(define ((correct-user-exn? msg) e)
  (and (exn:fail:user? e)
       (equal?
        msg
        (exn-message e))))

(check-exn
 (correct-user-exn? "foo: expected integer between 0 and 100\n  in positional argument <x>")
 (lambda ()
   (define/command-line-options/surface
     #:program "foo"
     #:argv (list "bar")
     #:arguments
     [x (int-range/p 0 100)])
   x))

(check-exn
 (correct-user-exn? "foo: expected integer between 0 and 100\n  in variadic arguments")
 (lambda ()
   (define/command-line-options/surface
     #:program "foo"
     #:argv (list "bar")
     #:rest
     [xs (int-range/p 0 100)])
   xs))

(check-exn
 (correct-user-exn? "foo: expected integer between 0 and 100\n  in <x> argument to flag --flag")
 (lambda ()
   (define/command-line-options/surface
     #:program "foo"
     #:argv (list "--flag" "bar")
     #:options
     [x (choice/o #:required
                ["--flag" [x (int-range/p 0 100)] "set x" x])])
   x))
