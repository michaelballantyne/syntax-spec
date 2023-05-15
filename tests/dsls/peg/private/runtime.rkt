#lang racket/base

(provide (all-defined-out))

(require
  racket/performance-hint
  racket/match
  syntax/srcloc)

(struct failure [])
(define the-failure (failure))

(struct parse-result [index value] #:transparent)

(struct text-rep [str source ix ln col] #:transparent)

(define (make-text str [source #f] [initial-pos 0] [initial-line 1] [initial-column 0])
  (text-rep str source initial-pos initial-line initial-column))

(define (wrap-input in)
  (match in
    [(? string?) (make-text in)]
    [(? list?) in]
    [(? text-rep?) in]
    [_ (raise-argument-error 'parse "(or/c string? list?)" in)]))

(define (step-input* in count)
    (define s (text-rep-str in))
    (let loop ([count count]
               [ix (text-rep-ix in)]
               [ln (text-rep-ln in)]
               [col (text-rep-col in)])
      (if (> count 0)
          (let-values ([(ix^ ln^ col^) (step-input (string-ref s ix) ix ln col)])
            (loop (- count 1) ix^ ln col^))
          (text-rep s (text-rep-source in) ix ln col))))

(begin-encourage-inline
  (define (fail) (values the-failure (void)))

  (define (token-pred-rt p in)
    (if (pair? in)
        (let-values ([(res srcloc) (p (car in))])
          (if res
              (begin
                (if (first-token-srcloc)
                    (last-token-srcloc srcloc)
                    (begin
                      (first-token-srcloc srcloc)
                      (last-token-srcloc srcloc)))
                (values (cdr in) res))
              (fail)))
        (fail)))

  (define (step-input c ix ln col)
    (if (char=? c #\newline)
        (values (+ ix 1)
                (+ ln 1)
                0)
        (values (+ ix 1)
                ln
                (+ col 1))))

  (define (string-rt s in)
    (if (and (text-rep? in)
             (<= (+ (text-rep-ix in) (string-length s)) (string-length (text-rep-str in))))
        (let loop ([ix (text-rep-ix in)]
                   [ln (text-rep-ln in)]
                   [col (text-rep-col in)]
                   [s-ix 0])
          (if (< s-ix (string-length s))
              (let ([c (string-ref (text-rep-str in) ix)])
                (if (char=? c (string-ref s s-ix))
                    (let-values ([(ix ln col) (step-input c ix ln col)])
                      (loop ix ln col (+ s-ix 1)))
                    (fail)))
              (values (text-rep (text-rep-str in) (text-rep-source in) ix ln col) s)))
        (fail)))

  (define (char-pred-rt p in)
    (if (and (text-rep? in)
             (< (text-rep-ix in) (string-length (text-rep-str in))))
        (let ([c (string-ref (text-rep-str in) (text-rep-ix in))])
          (if (p c)
              (let-values
                  ([(ix ln col)
                    (step-input c (text-rep-ix in) (text-rep-ln in) (text-rep-col in))])
                (values (text-rep (text-rep-str in) (text-rep-source in) ix ln col) (void)))
              (fail)))
        (fail)))

  (define first-token-srcloc (make-parameter #f))
  (define last-token-srcloc (make-parameter #f))
  
  (define (src-span-rt p in)
    (if (text-rep? in)
        (let ([source (text-rep-source in)]
              [init-pos (text-rep-ix in)]
              [init-ln (text-rep-ln in)]
              [init-col (text-rep-col in)])
          (let-values ([(in^ res) (p in)])
            (if (failure? in^)
                (values in^ #f #f)
                (values
                 in^ res
                 (srcloc source
                         init-ln init-col
                         init-pos (- (text-rep-ix in^) init-pos))))))
        (parameterize ([first-token-srcloc #f]
                       [last-token-srcloc #f])
          (let-values ([(in^ res) (p in)])
            (if (failure? in^)
                (values in^ #f #f)
                (values in^ res
                        (build-source-location (first-token-srcloc)
                                               (last-token-srcloc)))
                )))))

  )