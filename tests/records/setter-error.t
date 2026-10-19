(define-record-type point (fields (mutable x)))
(point-x-set! 5 1)
---
stage = "executable"
stderr_contains = "Error: `point-x-set!` expected a record of type point: 5"
chez = "skip"
