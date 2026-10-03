(define (range n acc) (if (= n 0) acc (range (- n 1) (cons n acc))))
(car (reverse (range 100000 '())))
---
stdout = "100000"
