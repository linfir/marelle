(define (k x) (let ((c (lambda () x))) c))
(k 1)
---
stdout = "#<procedure c>"
