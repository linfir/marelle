(define (k x) (let ((c (lambda (y) (+ x y)))) c))
((k 1) 2)
