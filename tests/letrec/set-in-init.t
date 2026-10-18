(letrec* ((a (begin (set! b 1) 2))
          (b 3))
  a)
---
stage = "executable"
stderr_contains = "Error: `b` assigned before its definition"
chez = "skip"
