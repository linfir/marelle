(define (k lambda) (let ((f (lambda 5))) f))
(k (lambda (x) x))
---
stdout = "5"
