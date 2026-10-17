(let ((v (make-vector 2 0)))
  (vector-set! v 0 (lambda (x) (+ x 1)))
  (vector-set! v 1 (lambda (x) (* x 2)))
  ((vector-ref v 1) ((vector-ref v 0) 4)))
---
stdout = "10"
