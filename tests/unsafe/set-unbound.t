(define (entry)
  (declare unsafe)
  (set! x 1)
  2)
(entry)
---
stage = "compiler"
stderr_contains = "unknown symbol"
