#lang racket

(require hosted-minikanren)

;; Towards multi-language programming...

(defrel (route origin end path)
  (conde
    [(== origin end) (== path '())]
    [(fresh (hop remainder)
       (== path (cons (list origin hop) remainder))
       (absento origin remainder)
       (direct origin hop)
       (route hop end remainder))]))

(defrel (direct a b)
  (conde
    [(== a "BOS") (== b "SEA")]
    [(== a "HOU") (== b "SLC")]
    [(== a "SEA") (== b "DEN")]
    [(== a "SEA") (== b "BOS")]
    [(== a "DEN") (== b "HOU")]
    [(== a "SLC") (== b "SFO")]))

(run* (q) (route "SEA" "HOU" q))
