(define (swap p) (cons (cdr p) (car p)))
(swap (cons 1 2))
---
stdout = "(2 . 1)"
