(define (f x) (when (and (< 0 x) (< x 10)) x))
(cons (f 5) (f 0))
