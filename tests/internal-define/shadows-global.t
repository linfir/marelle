(define x 1)
(define (f) (define x 2) x)
(cons (f) x)
---
stdout = "(2 . 1)"
