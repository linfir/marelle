;; A self tail loop reuses its frame: no collection
(define (count n)
  (if (= n 0)
      0
      (count (- n 1))))
(count 1000000)
---
env = { SCHEME_GC_STATS = "1" }
stderr_contains = "GC: 0 collections"
stdout = "0"
chez = "skip"
gc_stress = "skip"
