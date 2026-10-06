(define (id x) x)
(let ((f (id (lambda (x) x)))) f)
---
stdout = "#<procedure>"
