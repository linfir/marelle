(let*-values (((a b) (values 1 2)) ((c) (values (+ a b))) ((a) a)) (list a b c))
---
stdout = "(1 2 3)"
