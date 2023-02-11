#lang racket/base

(provide
 expand-peg
 local-expand-peg)

(require
  ee-lib
  syntax/parse
  (for-template "forms.rkt")
  "env-reps.rkt"
  "syntax-classes.rkt")

(define (local-expand-peg stx)
  (with-scope sc
    (expand-peg stx)))

; (-> syntax? (values syntax?))
(define/hygienic (expand-peg stx) #:definition
  (syntax-parse stx #:literal-sets (peg-literals)
    [eps this-syntax]
    ; Core forms
    [(seq ~! e1:peg e2:peg)
     (define/syntax-parse e1^ (expand-peg #'e1))
     (define/syntax-parse e2^ (expand-peg #'e2))
     #'(seq e1^ e2^)]
    [(alt ~! e1:peg e2:peg)
     (define/syntax-parse e1^ (expand-peg #'e1))
     (define/syntax-parse e2^ (expand-peg #'e2))
     #'(alt e1^ e2^)]
    [(plain-alt ~! e1:peg e2:peg)
     (define/syntax-parse e1^ (expand-peg #'e1))
     (define/syntax-parse e2^ (expand-peg #'e2))
     #'(plain-alt e1^ e2^)]
    [(* ~! e:peg)
     (define/syntax-parse e^ (expand-peg #'e))
     #'(* e^)]
    [(! ~! e:peg)
     (define/syntax-parse e^ (expand-peg #'e))
     #'(! e^)]
    [(: ~! x:id e:peg)
     (when (not (current-def-ctx))
       (raise-syntax-error
        #f
        "variables may only be bound within an action (=>) form"
        this-syntax))
     (define/syntax-parse e^ (expand-peg #'e))
     (define/syntax-parse x^ (bind! #'x (racket-var)))
     #'(: x^ e^)]
    [(=> ~! pe:peg e:expr)
     (with-scope sc
       (define/syntax-parse pe^ (expand-peg (add-scope #'pe sc)))
       (define/syntax-parse e^ (local-expand (add-scope #'e sc) 'expression '() (current-def-ctx)))
       #'(=> pe^ e^))]
    [(text (~or c:char s:string))
     this-syntax]
    [(char e:expr)
     (define/syntax-parse e^ (local-expand #'e 'expression '() (current-def-ctx)))
     #'(char e^)]
    [(token e:expr)
     (define/syntax-parse e^ (local-expand #'e 'expression '() (current-def-ctx)))
     #'(token e^)]
    [(:src-span v:id ~! e:peg)
     (define/syntax-parse e^ (expand-peg #'e))
     (define/syntax-parse v^ (bind! #'v (racket-var)))
     #'(:src-span v^ e^)]

    ; Macro
    [(~or head:id (head:id . rest))
     #:do [(define binding (lookup #'head peg-macro?))]
     #:when binding
     (expand-peg
      (apply-as-transformer (lambda (stx) (peg-macro-transform binding stx))
                              #'head
                              'expression
                              stx))]

    ; Introduce #%peg-datum implicit
    [(~or d:char d:string d:number)
     (with-syntax ([#%peg-datum (datum->syntax this-syntax '#%peg-datum)])
       (expand-peg #'(#%peg-datum d)))]

    ; Non-terminal reference
    [name:nonterm-id
     this-syntax]

    [_ (raise-syntax-error #f "not a peg form" this-syntax)]))
