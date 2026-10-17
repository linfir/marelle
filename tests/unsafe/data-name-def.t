(define-data f 1)

(define (f)
  (declare unsafe)
  1)

(define (entry)
  (declare unsafe)
  (f))
(entry)
---
stage = "compiler"
stderr_contains = "redefinition not allowed"
