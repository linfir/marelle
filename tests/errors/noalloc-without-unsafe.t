(define (f x) (declare noalloc) x)
1
---
stage = "compiler"
stderr_contains = "raw values or noalloc without unsafe"
chez = "skip"
