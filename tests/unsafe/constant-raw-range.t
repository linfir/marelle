(define-constant x (raw (ash 1 64)))
(define (entry)
  (declare unsafe)
  0)
(entry)
---
stage = "compiler"
stderr_contains = "invalid raw constant"
