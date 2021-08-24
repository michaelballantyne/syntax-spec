#lang racket/base

(provide deconstruct
         reconstruct) 

(require racket/match
         "spec.rkt")

(define (maybe-syntax-e stx)
  (if (syntax? stx) (syntax-e stx) stx))

;; syntax, pat -> (hashof symbol? syntax?)
(define (deconstruct stx pat)
  (define res (hash))
  (define (add-pvar! sym stx)
    (when (hash-has-key? res sym)
      (error 'deconstruct
             "duplicate pattern variable ~a"
             sym))
    (set! res (hash-set res sym stx)))
  
  (let rec ([stx stx]
            [pat pat]
            [last-stx stx])
    (match pat
      [(pany)
       (void)]
      [(pvar s)
       (add-pvar! s (if (syntax? stx) stx (datum->syntax last-stx stx last-stx last-stx)))]
      [(cons p1 p2)
       (define v (maybe-syntax-e stx))
       (when (not (pair? v))
         (raise-syntax-error
          #f
          (format "expected pair, found ~a"
                  (syntax->datum stx))
          stx))
       (rec (car v) p1 (if (syntax? stx) stx last-stx))
       (rec (cdr v) p2 (if (syntax? stx) stx last-stx))]
      [lit
       (define v (if (syntax? stx) (syntax->datum stx) stx))
       (when (not (equal? lit v))
         (raise-syntax-error
          #f
          (format "expected literal ~a, found ~a"
                  v
                  (syntax->datum stx))
          stx))]))
  res)

(module+ test
  (require rackunit
           racket/list)
  
  (define stx1
    #'(let ([x 5]) (+ x x)))
  (define pat1
    `(,(pany) ([,(pvar 'v) ,(pvar 'e)]) ,(pvar 'b)))

  ;; check the hash has the expected elements
  (let ()
    (define-values
      (v-expected e-expected b-expected)
      (syntax-case stx1 ()
        [(_ ([v e]) b)
         (values #'v #'e #'b)]))
    (check-equal?
     (deconstruct stx1 pat1)
     (hash 'v v-expected
           'e e-expected
           'b b-expected))))

;; (hashof symbol? syntax?), syntax, pat -> syntax
(define (reconstruct exp-state orig-stx pat)
  (let rec ([orig-stx orig-stx]
            [pat pat])
    (match pat
      [(pany)
       orig-stx]
      [(pvar s)
       (hash-ref exp-state s)]
      [(cons p1 p2)
       (define orig-v (maybe-syntax-e orig-stx))
       (define new-v
         (cons (rec (car orig-v) p1)
               (rec (cdr orig-v) p2)))
       (if (syntax? orig-stx)
           (datum->syntax orig-stx new-v orig-stx orig-stx)
           new-v)]
      [lit
       orig-stx])))

(module+ test
  ;; check that deconstruct . reconstruct produces at
  ;; least syntax that is datum equal to the original.
  (let ()
    (define r1
      (reconstruct
       (deconstruct stx1 pat1)
       stx1
       pat1))
    (check-equal?
     (syntax->datum r1)
     (syntax->datum stx1))))