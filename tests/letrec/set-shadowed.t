(let ((set! 1)) (letrec ((f (lambda () set!))) (f)))
---
stdout = "1"
