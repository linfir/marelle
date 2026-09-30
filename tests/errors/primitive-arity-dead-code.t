(if #t 1 (+ 1))
---
stage = "compiler"
stderr_contains = "wrong number of arguments"
chez = "skip"
