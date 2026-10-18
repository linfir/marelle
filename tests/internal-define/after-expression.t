(let ((a 0)) (set! a 1) (define x 2) x)
---
stage = "compiler"
stderr_contains = "invalid context for definition"
