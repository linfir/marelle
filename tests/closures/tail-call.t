(define (f n) ((lambda (x) (+ x n)) 1))
(f 41)
---
stdout = "42"
