(define (f x) (lambda (x) x))
((f 1) 2)
---
stdout = "2"
