(let ((x 1))
  (set! x 2))
---
stdout = "#<undef>"
chez = "skip"
