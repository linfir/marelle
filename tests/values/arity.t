(call-with-values (lambda () (values 1 2)) (lambda (a) a))
---
stage = "executable"
stderr = "Error: procedure of arity 1 called with 2 arguments\n"
chez = "skip"
