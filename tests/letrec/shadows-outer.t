(let ((f 1)) (letrec ((f (lambda (n) (if (= n 0) 0 (f (- n 1)))))) (f 5)))
---
stdout = "0"
