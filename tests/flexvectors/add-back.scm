(let ((fv (make-flexvector 0)))
  (flexvector-add-back! fv 1)
  (flexvector-add-back! fv 2)
  (flexvector->vector fv))
