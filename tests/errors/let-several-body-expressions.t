(let ((x 1)) x x)
---
stage = "compiler"
stderr_contains = "invalid let"
chez = "skip"
