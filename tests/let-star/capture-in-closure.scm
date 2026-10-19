(define (f x) (let* ((a x) (b (+ a 1))) (lambda () (+ a b))))
((f 20))
