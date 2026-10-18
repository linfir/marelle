(define (f) (set! x 1))
(f)
(define x 2)
x
---
stage = "executable"
stderr_contains = "Error: `x` assigned before its definition"
chez = "skip"
