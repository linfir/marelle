(define-data counter 0)

(define (entry)
  (declare unsafe)
  1)
(entry)
---
stage = "compiler"
stderr_contains = "invalid define-data"
