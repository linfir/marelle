(define (make-adder n) (lambda (x) (+ x n)))
((make-adder 40) 2)
---
stdout = "42"
