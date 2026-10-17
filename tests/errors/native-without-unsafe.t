(define (f x) (declare native) x)
1
---
stage = "compiler"
stderr_contains = "raw values or native without unsafe"
chez = "skip"
