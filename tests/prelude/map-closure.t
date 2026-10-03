(let ((k 10)) (map (lambda (x) (+ x k)) (cons 1 (cons 2 (cons 3 '())))))
---
stdout = "(11 12 13)"
