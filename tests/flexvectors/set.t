(let ((fv (flexvector 1 2 3)))
  (flexvector-set! fv 1 5)
  (flexvector->vector fv))
---
stdout = "#(1 5 3)"
