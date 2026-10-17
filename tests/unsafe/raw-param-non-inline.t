(define (f (raw x))
  (declare unsafe)
  #t)

(define (entry)
  (declare unsafe)
  (f (raw 1)))
(entry)
---
stage = "compiler"
stderr_contains = "raw parameter or result in a non-inline def"
