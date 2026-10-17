(set! car 1)
---
stage = "compiler"
stderr_contains = "set! of a library procedure or primitive"
chez = "skip"
