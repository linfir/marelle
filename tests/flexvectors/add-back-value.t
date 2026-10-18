;; flexvector-add-back! returns its flexvector, also as a procedure value
(let ((fv (flexvector)))
  (list (eq? (flexvector-add-back! fv 1) fv)
        (eq? (apply flexvector-add-back! fv (list 2 3)) fv)
        (flexvector->list fv)))
---
stdout = "(#t #t (1 2 3))"
