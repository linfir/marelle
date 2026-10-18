(define (f) (define a 1) (define b (+ a 1)) (* a b))
(f)
---
stdout = "2"
