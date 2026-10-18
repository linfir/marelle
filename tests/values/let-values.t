(let-values (((a b) (values 1 2)) ((c) (values 3))) (list a b c))
---
stdout = "(1 2 3)"
