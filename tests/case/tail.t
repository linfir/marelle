(define (count n) (case n ((0) 42) (else (count (- n 1)))))
(count 1000000)
---
stdout = "42"
