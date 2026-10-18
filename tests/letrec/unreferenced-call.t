(define (f)
  (define x (car 1))
  2)
(f)
---
stage = "executable"
stderr_contains = "Error: `car` expected a pair: 1"
chez = "skip"
