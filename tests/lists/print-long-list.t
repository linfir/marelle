(define (range n acc) (if (= n 0) acc (range (- n 1) (cons n acc))))
(range 100000 '())
---
stdout_contains = "99999 100000)"
