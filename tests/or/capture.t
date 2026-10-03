(define (f x) (lambda (y) (or x y)))
(cons ((f #f) 2) ((f 1) 2))
---
stdout = "(2 . 1)"
