(letrec* ((a 1) (b (+ a 1)) (c (* b 10))) (cons a (cons b c)))
---
stdout = "(1 2 . 20)"
