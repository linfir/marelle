(define (f) (define-values (a b) (values 1)) a)
(f)
---
stage = "executable"
stderr = "Error: procedure of arity 2 called with 1 arguments\n"
chez = "skip"
