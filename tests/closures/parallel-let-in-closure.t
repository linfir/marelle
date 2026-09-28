(define (f x) (lambda () (let ((x 1) (y x)) (cons x y))))
((f 5))
---
stdout = "(1 . 5)"
