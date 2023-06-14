#lang racket/base

(require db sql)

(define c (sqlite3-connect #:database 'memory))

(query-exec
 c
 (create-table #:temporary flights
               #:columns [flightfrom text] [flightto text]))

(query-exec
 c
 (insert #:into flights #:set [flightfrom "bos"] [flightto "slc"]))
(query-exec
 c
 (insert #:into flights #:set [flightfrom "bos"] [flightto "lax"]))
(query-exec
 c
 (insert #:into flights #:set [flightfrom "lax"] [flightto "slc"]))


(define where1 (scalar-expr-qq (= flightsfrom "bos")))

(displayln
 (query-rows
 c
 (select flightfrom flightto
         #:from flights
         #:where (and ,where1 (= flightto "slc")))))

