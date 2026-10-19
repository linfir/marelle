(define (make . xs) (lambda () xs))
((make 1 2 3))
