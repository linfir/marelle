(define (f x) (cdr x) x)
(f 3)
---
stage = "executable"
stderr_contains = "is_pair"
