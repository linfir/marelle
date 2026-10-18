(list (reduce + 0 (list 1 2 3)) (reduce + 0 '()) (reduce list 0 (list 1)) (reduce list 0 (list 1 2 3)))
---
stdout = "(6 0 1 (3 (2 1)))"
