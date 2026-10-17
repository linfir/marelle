(let ((v (make-vector 2 7)))
  (+ (vector-ref v 0) (vector-ref v 1)))
---
stdout = "14"
