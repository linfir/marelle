(apply car (list 1 2))
---
stage = "executable"
stderr_contains = "Error: procedure of arity 1 called with 2 arguments\n"
