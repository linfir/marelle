(let* ((a (list 1 2)) (r (append a (list 3))))
  (list r (eq? r a) (eq? (cdr r) (cdr a)) a))
---
stdout = "((1 2 3) #f #f (1 2))"
