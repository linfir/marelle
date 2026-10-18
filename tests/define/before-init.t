(define a b)
(define b 1)
a
---
stage = "executable"
stderr_contains = "Error: `b` used before its definition"
chez = "skip"
