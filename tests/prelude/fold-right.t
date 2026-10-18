(list (fold-right cons '() (list 1 2 3)) (fold-right list 0 (list 1 2 3)) (fold-right + 0 '()))
---
stdout = "((1 2 3) (1 (2 (3 0))) 0)"
