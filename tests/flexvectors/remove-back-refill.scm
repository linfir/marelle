;; Emptied, then filled again
(let ((fv (flexvector 1 2 3 4 5)))
  (let empty ((acc '()))
    (if (= (flexvector-length fv) 0)
        (begin
          (flexvector-add-back! fv 6 7)
          (cons acc (flexvector->vector fv)))
        (empty (cons (flexvector-remove-back! fv) acc)))))
