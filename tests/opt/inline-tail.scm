;; Inlined callees in tail and non-tail position, nested
(define (add1 x) (+ x 1))
(define (add2 x) (add1 (add1 x)))
(define (f x) (if (> x 0) (add2 x) (* 2 (add2 x))))
(list (f 1) (f -5))
