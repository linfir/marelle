(let ((x 1) (x 2)) x)
---
stage = "compiler"
stderr_contains = "duplicate let variable"
