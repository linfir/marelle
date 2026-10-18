(define (f) (define (g) 1) g)
(f)
---
stdout = "#<procedure g>"
