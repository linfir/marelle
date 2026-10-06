(set! car 1)
---
stage = "compiler"
stderr_contains = "set! of a prelude procedure or primitive"
chez = "skip"
