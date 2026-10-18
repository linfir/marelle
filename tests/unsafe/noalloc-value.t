(define (f x)
  (declare unsafe noalloc)
  x)

(define (entry)
  (declare unsafe)
  f)
(entry)
---
stage = "compiler"
stderr_contains = "noalloc def without a procedure object"
