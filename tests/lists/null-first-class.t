(let ((f null?)) (cons (f '()) (f 0)))
---
stdout = "(#t . #f)"
