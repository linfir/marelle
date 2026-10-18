(define (f) (define n 1) (set! n (+ n 1)) n)
(f)
---
stdout = "2"
