(define (loop n)
  (declare unsafe inline)
  (loop n))

(define (entry)
  (declare unsafe)
  (loop 1))
(entry)
---
stage = "compiler"
stderr_contains = "recursive inline def"
