((lambda (a b . r) a) 1)
---
stage = "executable"
stderr_contains = "Error: procedure of arity at least 2 called with 1 arguments\n"
