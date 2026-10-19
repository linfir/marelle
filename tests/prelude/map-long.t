(define (range n acc) (if (= n 0) acc (range (- n 1) (cons n acc))))
(length (map (lambda (x) x) (range 100000 '())))
---
stdout = "100000"
gc_stress = "skip"
