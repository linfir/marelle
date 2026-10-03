(define (f x) (lambda (y) (and x y)))
((f 1) 2)
---
stdout = "2"
