(letrec ((f (lambda () 1))) (define g (lambda () (+ (f) 1))) (g))
---
stdout = "2"
