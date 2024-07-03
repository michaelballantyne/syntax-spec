#lang typed/racket

(provide (all-defined-out))

(require
 racket/performance-hint
 racket/match
 syntax/srcloc)

(struct failure [])
(define the-failure (failure))

(struct (T) parse-result ([index : Integer] [value : T]) #:transparent)

(struct text-rep ([str : String] [source : (Union #f String)] [ix : Integer] [ln : Integer] [col : Integer]) #:transparent)

(: make-text (->* (String) (String Integer Integer Integer) text-rep))
(define (make-text str [source #f] [initial-pos 0] [initial-line 1] [initial-column 0])
  (text-rep str source initial-pos initial-line initial-column))

(: wrap-input (-> (Union String text-rep) text-rep))
(define (wrap-input in)
  (match in
    [(? string?) (make-text in)]
    [(? text-rep?) in]
    [_ (raise-argument-error 'parse "string?" in)]))

(begin-encourage-inline
  (define (fail) (values the-failure (void)))

  (: step-input (-> Char Integer Integer Integer (values Integer Integer Integer)))
  (define (step-input c ix ln col)
    (if (char=? c #\newline)
        (values (+ ix 1)
                (+ ln 1)
                0)
        (values (+ ix 1)
                ln
                (+ col 1))))

  (: string-rt (-> String text-rep (values (Union text-rep failure) (Union String Void))))
  (define (string-rt s in)
    (if (and (<= (+ (text-rep-ix in) (string-length s)) (string-length (text-rep-str in))))
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

  (: char-pred-rt (-> (-> Char Any) text-rep (values (Union text-rep failure) Void)))
  (define (char-pred-rt p in)
    (if (< (text-rep-ix in) (string-length (text-rep-str in)))
        (let ([c (string-ref (text-rep-str in) (text-rep-ix in))])
          (if (p c)
              (let-values
                  ([(ix ln col)
                    (step-input c (text-rep-ix in) (text-rep-ln in) (text-rep-col in))])
                (values (text-rep (text-rep-str in) (text-rep-source in) ix ln col) (void)))
              (fail)))
        (fail)))

  (: src-span-rt (All (T) (-> (-> text-rep (values text-rep T))
                              text-rep
                              (values text-rep (Union #f T) (Union #f srcloc))
                              )))
  (define (src-span-rt p in)
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
                     init-pos (- (text-rep-ix in^) init-pos))))))))
