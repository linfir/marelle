(define (make-adder n) (lambda (y) (+ n y)))
(define add2 (make-adder 2))
(add2 40)
