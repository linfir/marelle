(define (f x)
  (declare unsafe native)
  x)

(define (entry)
  (declare unsafe)
  f)
(entry)
---
stage = "compiler"
stderr_contains = "native def without a procedure object"
