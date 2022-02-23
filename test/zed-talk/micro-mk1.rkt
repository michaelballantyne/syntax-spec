

;; An example:

(run* (q)
    (and
     (exists (x)
       (or
        (== q (cons 1 x))
        (== q (cons 2 x))
        (== q (cons 3 x))))
     (exists (y)
       (or
        (== q (cons y 4))
        (== q (cons y 5))))))
;; =>
'((1 . 4) (1 . 5)
    (2 . 4) (2 . 5)
    (3 . 4) (3 . 5))


