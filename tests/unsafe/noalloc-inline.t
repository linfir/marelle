(define (f x)
  (declare unsafe noalloc inline)
  x)

(define (entry)
  (declare unsafe)
  (f 1))
(entry)
---
stage = "compiler"
stderr_contains = ":noalloc with another attribute"
