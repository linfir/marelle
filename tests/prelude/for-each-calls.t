(for-each car (cons 1 (cons 2 (cons 3 '()))))
---
stage = "executable"
stderr_contains = "is_pair"
chez = "skip"
