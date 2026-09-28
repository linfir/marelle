((lambda (x) 1 x) 2)
---
stage = "compiler"
stderr_contains = "invalid lambda"
chez = "skip"
