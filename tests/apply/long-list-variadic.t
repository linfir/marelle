(define (iota n acc) (if (= n 0) acc (iota (- n 1) (cons n acc))))
(length (apply (lambda (a . r) r) (iota 100000 (list))))
---
stdout = "99999"
gc_stress = "skip"
