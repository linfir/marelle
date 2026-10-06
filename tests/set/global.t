(define x 1)
(begin (set! x 2) x)
---
stdout = "2"
