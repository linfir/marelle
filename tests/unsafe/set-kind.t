(define (entry)
  (declare unsafe)
  (let ((x 1))
    (set! x (raw 2))
    x))
(entry)
---
stage = "compiler"
stderr_contains = "expected a val"
