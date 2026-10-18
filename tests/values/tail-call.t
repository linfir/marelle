(define (g x) (values x (+ x 1)))
(define (f x) (g (* x 2)))
(call-with-values (lambda () (f 5)) list)
---
stdout = "(10 11)"
