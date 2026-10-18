;; An inline call ending a def is checked against the def's result
(define (f x)
  (declare unsafe noalloc (result raw))
  (car x))

(define (entry)
  (declare unsafe)
  (as-val (f (cons 1 2))))
(entry)
---
stage = "compiler"
stderr_contains = "expected a raw"
