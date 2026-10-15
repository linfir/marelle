(define (count n)
  (if (= n 0)
      0
      (count (- n 1))))
(count 1000000)
---
env = { SCHEME_HEAP = "1000", SCHEME_GC_STATS = "1" }
stdout = "0"
stderr_contains = "GC: 0 collections"
chez = "skip"
gc_stress = "skip"
