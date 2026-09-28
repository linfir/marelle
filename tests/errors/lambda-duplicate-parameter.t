((lambda (x x) x) 1 2)
---
stage = "compiler"
stderr_contains = "duplicate parameter"
