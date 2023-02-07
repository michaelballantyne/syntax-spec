#lang racket/base

(require racket/list
         "../../testing.rkt")

(syntax-spec
  (binding-class pat-var)
  (extension-class pat-macro #:binding-space pm)
  (nonterminal/two-pass pat
    #:allow-extension pat-macro
    #:binding-space pm
    x:pat-var
    #:binding (export x)
    (var x:pat-var)
    #:binding (export x)
    (and2 p1:pat p2:pat)
    #:binding [(re-export p1) (re-export p2)]
    (? pred:racket-expr)
    (app proc:racket-expr p:pat)
    #:binding (re-export p)
    (not p:pat)
    ; you don't want to export from a `not`
    #:binding {(recursive p)})
  (nonterminal clause
    [p:pat body:racket-expr ...+]
    #:binding {(recursive p) body})
  (host-interface/expression
   (match target:racket-expr c:clause ...)
   #'(with-reference-compilers ([pat-var immutable-reference-compiler])
       (let ([target-pv target])
         (match-clauses target-pv c ...)))))

(define-syntax match-clauses
  (syntax-parser
    [(_ target:id c ...)
     (syntax-parse #'(c ...)
       [() #'(error 'match "no matching clause for ~a" target)]
       [([p body ...+] c ...)
        #'(do-match target p (begin body ...) (match-clauses target c ...))])]))

(define-syntax do-match
  (syntax-parser
    [(_ target:id pat on-success on-fail)
     (syntax-parse #'pat
       ; TODO something better then datum
       #:datum-literals (var and2 ? app)
       [x:id #'(let ([x target]) on-success)]
       [(var x:id) #'(let ([x target]) on-success)]
       [(and2 p1 p2)
        #'(do-match target p1 (do-match target p2 on-success on-fail) on-fail)]
       [(? pred)
        #'(if (pred target)
            on-success
            on-fail)]
       [(app proc p)
        #'(let ([new-target (proc target)])
            (do-match new-target p on-success on-fail))]
       [(not p)
        #'(do-match target p on-fail on-success)])]))

; TODO shouldn't have to mess with make-interned-syntax-introducer
(define-syntax define-match-expander
  (syntax-parser
    [(_ name:id trans:expr)
     #`(define-syntax #,((make-interned-syntax-introducer 'pm) #'name 'add)
         (pat-macro trans))]))

(define-match-expander _
  (syntax-parser
    [(~datum _) #'ignored]))
(define-match-expander and
  (syntax-parser
    [(_) #'_]
    [(_ p0 p ...)
     #'(and2 p0 (and p ...))]))
(define-match-expander cons
  (syntax-parser
    [(_ car-pat cdr-pat)
     #'(and (? cons?)
            (app car car-pat)
            (app cdr cdr-pat))]))
(define-match-expander equal?
  (syntax-parser
    [(_ val)
     #'(? (lambda (x) (equal? x val)))]))
(define-match-expander quote
  (syntax-parser
    [(_ lit)
     #'(equal? 'lit)]))
(define-match-expander list
  (syntax-parser
    [(_) #''()]
    [(_ p0 p ...)
     #'(cons p0 (list p ...))]))

(check-equal? (match 2 [x x]) 2)
(check-equal? (match 2 [(? odd?) 'bad] [(? even?) 'good]) 'good)
(check-equal? (match 2
                [(and2 (? odd?) (? even?)) 'bad]
                [(and2 (? even?) (? odd?)) 'bad]
                [(and2 (? even?) (? even?)) 'good])
              'good)
(check-equal? (match 2
                [(app add1 x) x])
              3)
(check-equal? (match 2
                [(not (? even?)) 'bad]
                [(not (? odd?)) 'good])
              'good)
; macro cons pattern
(check-equal? (match '(1 2)
                [(cons a _) a])
              1)
; doesn't shadow regular cons
(check-equal? (cons 1 '(2)) '(1 2))
; doesn't shadow regular cons in match body
(check-equal? (match '(1 2)
                [(cons a b) (cons a b)])
              '(1 2))
(check-equal? (match 1 [_ 2]) 2)
(check-equal? (match 1 ['2 'bad] ['1 'good])
              'good)
(check-equal? (match '(1 2)
                [(list a b) (list b a)])
              '(2 1))
(check-equal? (match '(1) [(var cons) cons]) '(1))
; currently an ambiguity error
;(check-equal? (match '(1) [(and (var cons) (cons a b)) (cons a b)]) '(1))
