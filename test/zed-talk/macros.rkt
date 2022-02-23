#lang racket

;;
;; Macros
;;


(* (+ 1 2) 2)


(define-syntax-rule
  (~> e1 (f arg ...) ...)
  ((apply compose
          (reverse
           (list
            (curryr f arg ...)
            ...)))
   e1))


(~> 1
    (+ 2)
    (* 2))

















;;
;; Procedural macros
;;

(require (for-syntax racket/base syntax/parse))

(define-syntax (match stx)
  (syntax-parse stx
    [(match e
       [pat rhs]
       ...)

     ;; Compile with arbitrary Racket code:
     ;;  - Parse
     ;;  - Check static semantics
     ;;  - Optimize
     ;;  - Generate code
     
     'TODO]))







