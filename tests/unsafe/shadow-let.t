(define (entry)
  (declare unsafe)
  (let ((x 1)
        (x 2))
    x))
(entry)
---
stage = "compiler"
stderr_contains = "duplicate let variable"
