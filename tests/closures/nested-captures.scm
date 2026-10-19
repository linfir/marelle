(define (f x) (lambda (y) (lambda (z) (cons x (cons y z)))))
(((f 1) 2) 3)
