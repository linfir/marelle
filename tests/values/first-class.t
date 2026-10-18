(define v values)
(call-with-values (lambda () (v 1 2)) list)
---
stdout = "(1 2)"
