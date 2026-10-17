;; A self tail loop in a small heap: each call gets a new frame, the old one is garbage
(define (count n)
  (if (= n 0)
      0
      (count (- n 1))))
(count 1000000)
---
env = { SCHEME_HEAP = "1000" }
stdout = "0"
chez = "skip"
gc_stress = "skip"
