(define (f x) (lambda () (let ((y 1)) x y)))
((f (quote ())))
