;; The old storages are garbage: 10^5 elements, many collections in a small heap
(define fv (make-flexvector 0))
(let fill ((i 0))
  (when (< i 100000)
    (flexvector-add-back! fv (list i))
    (fill (+ i 1))))
(car (flexvector-ref fv 99999))
---
stdout = "99999"
env = { SCHEME_HEAP = "1000000" }
