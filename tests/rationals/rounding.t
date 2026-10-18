(define xs (list 5/2 7/2 -5/2 -7/2 1/3 2/3 -1/3 -2/3 3 -3))
(list (map floor xs) (map ceiling xs) (map truncate xs) (map round xs) (map round-half-up xs))
---
stdout = "((2 3 -3 -4 0 0 -1 -1 3 -3) (3 4 -2 -3 1 1 0 0 3 -3) (2 3 -2 -3 0 0 0 0 3 -3) (2 4 -2 -4 0 1 0 -1 3 -3) (3 4 -2 -3 0 1 0 -1 3 -3))"
