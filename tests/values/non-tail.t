(define (g x) (values x (+ x 1)))
(define (f x) (let ((r (g x))) r))
(call-with-values (lambda () (f 5)) list)
---
stdout = "(5 6)"
chez = "skip"
