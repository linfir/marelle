(define (outer x) (lambda () (lambda () x)))
(((outer 7)))
---
stdout = "7"
