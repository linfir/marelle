(define (call f) (f 1 2 3))
(cons (call list) (call (lambda (a . r) r)))
---
stdout = "((1 2 3) 2 3)"
