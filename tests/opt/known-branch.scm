;; A comparison of two literals goes to its branch
(define (f x) (if (< 1 2) x (car x)))
(f 5)
