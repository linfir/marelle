;; More than the largest semispace, 2^26 words
(vector-length (make-vector 70000000))
---
stage = "executable"
stderr_contains = "Out of memory"
chez = "skip"
