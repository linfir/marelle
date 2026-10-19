(define (adder n) (lambda (x) `(,n + ,x = ,(+ n x))))
((adder 2) 3)
