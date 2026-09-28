(define (f 1) 1)
(f 2)
---
stage = "compiler"
stderr_contains = "invalid parameter"
