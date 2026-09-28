(define (square x) (* x x))
(define (f n) (lambda (x) (+ (square x) n)))
((f 1) 3)
---
stdout = "10"
