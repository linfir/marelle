(define (compose f g) (lambda (x) (f (g x))))
(define (inc x) (+ x 1))
(define (dbl x) (* x 2))
((compose inc dbl) 5)
