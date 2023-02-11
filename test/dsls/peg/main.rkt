#lang racket/base

(require
  "core.rkt"
  syntax/srcloc
  (for-syntax racket/base syntax/parse))

(provide
  (except-out (all-from-out "core.rkt") seq alt)
  (struct-out parse-result)
  define-peg-syntax-parser
  (rename-out
    [seq* seq]
    [alt* alt])
  ?
  any-char
  char-range

  symbol-token
  string-token
  predicate-token
  syntax-token

  use-literal-token-interpretation)

(define-syntax-rule
  (define-peg-syntax-parser name clause ...)
  (define-syntax name
    (peg-macro
      (syntax-parser
        clause ...))))

(define-peg-syntax-parser seq*
  [(_ p:peg) #'p]
  [(_ p1:peg p+:peg ...+)
   #'(seq p1 (seq* p+ ...))])

(define-peg-syntax-parser alt*
  [(_ p:peg) #'p]
  [(_ p1:peg p+:peg ...+)
   #'(alt p1 (alt* p+ ...))])

(define-peg-syntax-parser ?
  [(_ p:peg) #'(alt p eps)])

(define-peg-syntax-parser any-char
  [_:id #'(char (lambda (x) #t))])

(define-peg-syntax-parser char-range
  [(_ lower:character upper:character)
   #'(char (lambda (c) (and (>= lower c) (<= c upper))))])

(define-peg-syntax-parser symbol-token
  [(_ (~or v:string v:id))
   (define/syntax-parse s (if (symbol? (syntax-e #'v))
                            #'v
                            (datum->syntax #'v (string->symbol (syntax-e #'v)))))
   #'(token (lambda (t) (values (and (eq? t 's) t) #f)))])

(define-peg-syntax-parser string-token
  [(_ s:string)
   #'(token (lambda (t) (values (and (eq? t 's) t) #f)))])

(define-peg-syntax-parser syntax-token
  [(_ x:string)
   (define/syntax-parse x-sym (string->symbol (syntax->datum #'x)))
   #'(token (lambda (s)
              (if (equal? 'x-sym (syntax-e s))
                (values s (build-source-location s))
                (values #f #f))))])

(define-peg-syntax-parser predicate-token
  [(_ e:expr)
   #'(token (lambda (t) (values (and (e t) t) #f)))])

(define-syntax use-literal-token-interpretation
  (syntax-parser
    [(_ id)
     (define/syntax-parse this-#%peg-datum (datum->syntax this-syntax '#%peg-datum))
     #'(define-syntax this-#%peg-datum (make-rename-transformer #'id))]))

