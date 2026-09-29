(define (f) 1 2)
(f)
---
stage = "compiler"
stderr_contains = "define: invalid form"
chez = "skip"
