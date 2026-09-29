(let ((1 2)) 3)
---
stage = "compiler"
stderr_contains = "invalid let binding"
