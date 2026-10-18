(define v (make-vector 3 0))
(do ((i 0 (+ i 1))) ((= i 3) v) (vector-set! v i (* i i)))
---
stdout = "#(0 1 4)"
