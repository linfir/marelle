(if #t (define x 1) 2)
---
stage = "compiler"
stderr_contains = "invalid context for definition"
