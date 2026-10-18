(let () (define x 1) (define x 2) x)
---
stage = "compiler"
stderr_contains = "duplicate"
