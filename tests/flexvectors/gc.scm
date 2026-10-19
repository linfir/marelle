;; Pairs in a growing flexvector, through collections: the storage is traced
(define fv (make-flexvector 0))
(let fill ((i 0))
  (when (< i 2000)
    (flexvector-add-back! fv (cons i (make-vector 3 i)))
    (fill (+ i 1))))
(let sum ((i 0)
          (acc 0))
  (if (= i (flexvector-length fv))
      acc
      (let ((x (flexvector-ref fv i)))
        (sum (+ i 1) (+ acc (car x) (vector-ref (cdr x) 2))))))
