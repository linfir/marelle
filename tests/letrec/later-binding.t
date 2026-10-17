(letrec ((f (lambda () g)) (g 5)) (f))
---
stdout = "5"
