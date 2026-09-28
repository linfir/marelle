(define (f a b c d e f g) a)
(f 1 2 3 4 5 6 7)
---
stage = "compiler"
stderr_contains = "too many parameters"
chez = "skip"
