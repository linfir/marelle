(define (f x x)
  (declare unsafe)
  x)

(define (entry)
  (declare unsafe)
  (f 1 2))
(entry)
---
stage = "compiler"
stderr_contains = "duplicate parameter"
