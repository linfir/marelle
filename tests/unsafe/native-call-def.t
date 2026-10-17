(define (g x)
  (declare unsafe)
  x)

(define (f x)
  (declare unsafe native)
  (g x))

(define (entry)
  (declare unsafe)
  (f 1))
(entry)
---
stage = "compiler"
stderr_contains = "safepoint or self in a native def"
