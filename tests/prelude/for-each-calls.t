(for-each car (cons 1 (cons 2 (cons 3 '()))))
---
stage = "executable"
stderr_contains = "Error: `car` expected a pair: 1\n"
chez = "skip"
