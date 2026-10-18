(define (f)
  (declare unsafe)
  (let ((load (lambda (x) x)))
    (load 5)))
(f)
---
stdout = "5"
chez = "skip"
