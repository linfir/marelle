(define-record-type point (fields x y))
(define-record-type other (fields x y))
(point-y (make-other 1 2))
---
stage = "executable"
stderr_contains = "Error: `point-y` expected a record of type point: #[other 1 2]"
chez = "skip"
