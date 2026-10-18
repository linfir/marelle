(define (f)
  (define (g) x)
  (define y (g))
  (define x 1)
  y)
(f)
---
stage = "executable"
stderr_contains = "Error: `x` used before its definition"
chez = "skip"
