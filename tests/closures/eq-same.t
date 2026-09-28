(define (make-adder n) (lambda (x) (+ x n)))
(let ((f (make-adder 1))) (eq? f f))
---
stdout = "#t"
