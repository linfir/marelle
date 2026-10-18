(let* ((fv (flexvector 1 2 3))
       (x (flexvector-remove-back! fv)))
  (cons x (flexvector->vector fv)))
---
stdout = "(3 . #(1 2))"
