;; Each operand is evaluated once
(define n 0)
(define (next) (set! n (+ n 1)) n)
(cons (< 0 (next) 5) n)
---
stdout = "(#t . 1)"
