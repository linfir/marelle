(let ((x 1)) (set! x))
---
stage = "compiler"
stderr_contains = "invalid set!"
chez = "skip"
