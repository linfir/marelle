;; An index equal to the length appends
(let ((fv (flexvector 1 2)))
  (flexvector-set! fv 2 3)
  (flexvector->vector fv))
