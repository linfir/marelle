(define (f))
(f)
---
stage = "compiler"
stderr_contains = "define: invalid form"
