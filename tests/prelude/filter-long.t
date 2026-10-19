(define (iota n acc) (if (= n 0) acc (iota (- n 1) (cons n acc))))
(length (filter even? (iota 100000 '())))
---
stdout = "50000"
gc_stress = "skip"
