(let ((fv (flexvector 1 2 3)))
  (flexvector-clear! fv)
  (flexvector-add-back! fv 4)
  (flexvector->vector fv))
---
stdout = "#(4)"
