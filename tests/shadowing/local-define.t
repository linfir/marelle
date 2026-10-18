(define (f define) (define 3))
(f (lambda (x) (+ x 1)))
---
stdout = "4"
