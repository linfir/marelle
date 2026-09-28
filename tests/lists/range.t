(define (range n acc) (if (= n 0) acc (range (- n 1) (cons n acc))))
(range 5 '())
---
stdout = "(1 2 3 4 5)"
