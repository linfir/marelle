(if #t (define-record-type point (fields x)) 1)
---
stage = "compiler"
stderr_contains = "invalid context for definition"
chez = "skip"
