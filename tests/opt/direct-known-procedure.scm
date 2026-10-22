;; A procedure passed to an inlined callee is then called directly
(define (twice h x) (h (h x)))
(define (g x) (* x 3))
(define (run n) (twice g n))
(run 2)
