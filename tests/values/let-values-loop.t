(let loop ((i 1000000) (acc 0))
  (if (= i 0)
      acc
      (let-values (((a b) (values i 1)))
        (loop (- a b) (+ acc 1)))))
---
env = { SCHEME_HEAP = "5000", SCHEME_GC_STATS = "1" }
stdout = "1000000"
stderr_contains = " max live words\n"
chez = "skip"
