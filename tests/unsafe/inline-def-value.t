(define (id x)
  (declare unsafe inline)
  x)

(define (entry)
  (declare unsafe)
  (let ((f id))
    (f 1)))
(entry)
---
stage = "compiler"
stderr_contains = "inline def without a procedure object"
