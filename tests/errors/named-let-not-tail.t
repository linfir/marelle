(let loop ((i 0)) (+ 1 (loop i)))
---
stage = "compiler"
stderr_contains = "called not in exit position"
chez = "skip"
