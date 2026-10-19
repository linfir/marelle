(define (adder n) (case-lambda ((x) (+ n x)) ((x y) (+ n x y))))
(let ((a (adder 10))) (list (a 1) (a 1 2)))
