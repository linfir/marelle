(let* ((l (list 1 2 3)) (c (list-copy l))) (list c (eq? l c) (list-copy '())))
---
stdout = "((1 2 3) #f ())"
