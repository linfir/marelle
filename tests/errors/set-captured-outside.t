(let ((x 1))
  (let ((f (lambda () x)))
    (set! x 2)
    (f)))
---
stage = "compiler"
stderr_contains = "set! of a captured variable"
chez = "skip"
