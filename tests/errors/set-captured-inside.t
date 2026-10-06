(let ((x 1))
  (lambda () (set! x 2)))
---
stage = "compiler"
stderr_contains = "set! of a captured variable"
chez = "skip"
