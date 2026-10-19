(define (f x) (lambda (y) (let ((x y)) (+ x x))))
((f 100) 3)
