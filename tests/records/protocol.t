(define-record-type point (fields x) (protocol (lambda (new) (lambda () (new 0)))))
(point-x (make-point))
---
stage = "compiler"
stderr_contains = "invalid define-record-type"
chez = "skip"
