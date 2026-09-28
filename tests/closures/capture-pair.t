(define (f p) (lambda () (car p)))
((f (cons 1 2)))
---
stdout = "1"
