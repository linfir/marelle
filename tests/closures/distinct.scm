(define (make-adder n) (lambda (x) (+ x n)))
(eq? (make-adder 1) (make-adder 1))
