(define (iota n acc) (if (= n 0) acc (iota (- n 1) (cons n acc))))
(fold-right + 0 (iota 100000 '()))
---
stdout = "5000050000"
gc_stress = "skip"
