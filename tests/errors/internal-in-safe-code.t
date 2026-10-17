(define (f x) (fixnum? x))
(f 1)
---
stage = "compiler"
stderr_contains = "unknown symbol"
chez = "skip"
