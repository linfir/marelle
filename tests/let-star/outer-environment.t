(let ((a 1)) (let* ((b a) (a 5)) (+ a b)))
---
stdout = "6"
