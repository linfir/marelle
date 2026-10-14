(define (f x) (cdr x) x)
(f 3)
---
stage = "executable"
stderr_contains = "Error: `cdr` expected a pair: 3\n"
