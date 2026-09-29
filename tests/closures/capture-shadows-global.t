(define (g) 1)
(define (f g) (lambda () g))
((f 5))
---
stdout = "5"
