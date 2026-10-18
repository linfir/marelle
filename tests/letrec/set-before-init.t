(define (f)
  (define (set-x!) (set! x 5))
  (define y (set-x!))
  (define x (+ x 1))
  x)
(f)
---
stage = "executable"
stderr_contains = "Error: `x` assigned before its definition"
chez = "skip"
