(define (f x)
  (set! x 2))
(f 1)
---
stdout = "#<undef>"
chez = "skip"
