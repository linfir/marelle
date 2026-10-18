(letrec ((x (+ 1 x))) x)
---
stage = "executable"
stderr_contains = "Error: `x` used before its definition"
chez = "skip"
