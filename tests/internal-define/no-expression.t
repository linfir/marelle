(let () (define x 2))
---
stage = "compiler"
stderr_contains = "no expression in body"
