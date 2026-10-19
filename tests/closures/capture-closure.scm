(define (make-adder n) (lambda (x) (+ x n)))

(define (twice f) (lambda (x) (f (f x))))
((twice (make-adder 3)) 10)
