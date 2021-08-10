#lang racket/base

(provide
 pany
 pvar)

;; pat is one of:
;;  (pany)
;;  (pvar symbol)
;;  (cons pat pat)
;;  any other value
;; a given pvar symbol must only occur once in a pattern.
(struct pany ())
(struct pvar (sym))
  
    


    