(define (sum-fields a b)
  (define-record-type pair2 (fields first (mutable second)))
  (let ((p (make-pair2 a b)))
    (pair2-second-set! p (* 2 (pair2-second p)))
    (+ (pair2-first p) (pair2-second p))))
(sum-fields 1 20)
---
stdout = "41"
