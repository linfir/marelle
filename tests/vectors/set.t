(let ((v (make-vector 3 0)))
  (vector-set! v 0 #\a)
  (vector-set! v 2 (cons 1 2))
  v)
---
stdout = "#(#\\a 0 (1 . 2))"
