(define n 0)
(define (inc!) (set! n (+ n 1)))
(begin (inc!) (inc!) (inc!) n)
---
stdout = "3"
