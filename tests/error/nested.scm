(define (f x) (if (< x 0) (error (quote f) "negative" x) x))
(+ (f 1) (f -2))
