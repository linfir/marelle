(define (range n acc) (if (= n 0) acc (range (- n 1) (cons n acc))))
(define (sum l acc) (if (null? l) acc (sum (cdr l) (+ acc (car l)))))
(sum (range 100 '()) 0)
---
stdout = "5050"
