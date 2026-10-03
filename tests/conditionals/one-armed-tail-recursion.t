(define (count n)
  (if (> n 0) (count (- n 1))))
(count 1000000)
---
stdout = "#<undef>"
chez = "skip"
