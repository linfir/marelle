(car 1)
(define x (cdr 2))
x
---
stage = "executable"
stderr_contains = "Error: `car` expected a pair: 1"
chez = "skip"
