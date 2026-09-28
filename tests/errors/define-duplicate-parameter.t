(define (f x x) x)
(f 1 2)
---
stage = "compiler"
stderr_contains = "duplicate parameter"
