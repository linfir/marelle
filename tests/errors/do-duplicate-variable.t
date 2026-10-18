(do ((i 0) (i 1)) (#t 1))
---
stage = "compiler"
stderr_contains = "duplicate do variable"
chez = "skip"
