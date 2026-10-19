(define-constant x (+ y 1))
(define (entry)
  (declare unsafe)
  x)
(entry)
---
stage = "compiler"
stderr_contains = "invalid constant expression"
