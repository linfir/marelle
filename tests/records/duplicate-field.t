(define-record-type point (fields x (mutable x)))
1
---
stage = "compiler"
stderr_contains = "duplicate field"
chez = "skip"
