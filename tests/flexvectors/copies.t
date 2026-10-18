;; The conversions copy the elements
(let* ((v (vector 1 2))
       (fv (vector->flexvector v))
       (w (flexvector->vector fv)))
  (vector-set! v 0 3)
  (flexvector-set! fv 1 4)
  (list v (flexvector->vector fv) w))
---
stdout = "(#(3 2) #(1 4) #(1 2))"
