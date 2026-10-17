(let loop ((i 0)) (lambda () (loop i)))
---
stage = "compiler"
stderr_contains = "named let called from a lambda"
chez = "skip"
