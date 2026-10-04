(define (f) 1)
(define f 2)
f
---
stage = "compiler"
stderr_contains = "redefinition not allowed"
chez = "skip"
