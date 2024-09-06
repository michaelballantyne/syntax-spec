#lang racket

(provide download-flights-csv create-flights-table query-flight-rows)

(require hosted-minikanren
         hosted-minikanren/demos/icfp2024/facts
         csv-reading
         racket/list
         net/url
         net/url-string
         db sql)

(define SOURCE
  "https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat")

;; () -> [Listof [Listof String String]]
;; Should produce a list of flights, each flight a list matching the data schema
(define (download-flights-csv)
  (for/list ([row (csv->list (get-pure-port (string->url SOURCE)))])
    (list (third row) (fifth row))))

(define (create-flights-table v)
  (void))

(define-facts-table flights [flightfrom flightto]
  #:initial-data (download-flights-csv))

(defrel (direct a b)
  (query-facts flights a b))

(define (query-flight-rows conn a-arg b-arg)
  (run* (a b)
    (== a a-arg)
    (== b b-arg)
    (direct a b)))