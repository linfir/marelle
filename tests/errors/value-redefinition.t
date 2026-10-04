(define x 1)
(define x 2)
x
---
stage = "compiler"
stderr_contains = "redefinition not allowed"
chez = "skip"
