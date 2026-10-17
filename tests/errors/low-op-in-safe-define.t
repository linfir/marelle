(define (f x) (declare public) (raw+ x x))
1
---
stage = "compiler"
stderr_contains = "unknown symbol"
chez = "skip"
