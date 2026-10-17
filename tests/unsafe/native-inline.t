(define (f x)
  (declare unsafe native inline)
  x)

(define (entry)
  (declare unsafe)
  (f 1))
(entry)
---
stage = "compiler"
stderr_contains = ":native with another attribute"
