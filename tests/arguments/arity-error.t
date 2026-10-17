((lambda (a b c d e f g) a) 1 2 3 4 5 6 7 8)
---
stage = "executable"
stderr_contains = "Error: procedure of arity 7 called with 8 arguments\n"
