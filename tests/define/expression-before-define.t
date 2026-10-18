(define v (make-vector 2 0))
(vector-set! v 0 1)
(define w v)
w
---
stdout = "#(1 0)"
