(define (f x)
  (lambda (x) (lambda () x)))
(((f 1) 2))
---
stdout = "2"
