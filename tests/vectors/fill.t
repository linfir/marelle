(let ((v (make-vector 3 1)))
  (vector-fill! v #f)
  v)
---
stdout = "#(#f #f #f)"
