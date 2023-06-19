#lang racket
(require "mk.rkt")
(require json)
(require net/url)
;; AviationStack


(define API-KEY "5fc685009d84a0372cf595f648066926")
(define URL (string->url "http://api.aviationstack.com/v1/flights"))

(define params
  '((access_key . "5fc685009d84a0372cf595f648066926")
    (limit . "100")
    (dep_iata . "BOS")
    (arr_iata . "SLC"))
  )
(define (make-params from to)
  '((access_key . "5fc685009d84a0372cf595f648066926")
    (limit . "100")
    (dep_iata . "BOS")
    (arr_iata . "SLC"))
  )

(define OUR-QUERY (struct-copy url URL [query params]))

(define QR (call/input-url OUR-QUERY
                get-pure-port
                port->string))

;; [Listof Hash]
(define JSIFIED-QUERY-DATA (hash-ref (string->jsexpr QR) 'data))

(define (get-vals loh)
  (for/list ((h (in-list loh)))
    (define arriv (hash-ref h 'arrival))
    (define depts (hash-ref h 'departure))
    (define arriv-iata (hash-ref arriv 'iata))
    (define depts-iata (hash-ref depts 'iata))
    (cons arriv-iata depts-iata)))
