(if (letrec ((t (lambda () #f))) (t)) 1 2)
---
stdout = "2"
