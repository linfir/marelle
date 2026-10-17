(cond (1 => car cdr))
---
stage = "compiler"
stderr_contains = "invalid => clause"
chez = "skip"
