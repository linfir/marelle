(define (iota n acc) (if (= n 0) acc (iota (- n 1) (cons n acc))))
(apply + (iota 100000 (list)))
---
stdout = "5000050000"
