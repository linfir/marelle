;; Many doublings of the storage, then the sum of the elements
(let ((fv (make-flexvector 0)))
  (let fill ((i 0))
    (when (< i 1000)
      (flexvector-add-back! fv i)
      (fill (+ i 1))))
  (let sum ((i 0)
            (acc 0))
    (if (= i (flexvector-length fv))
        acc
        (sum (+ i 1) (+ acc (flexvector-ref fv i))))))
