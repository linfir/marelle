(list (list? '()) (list? (list 1)) (list? (list 1 2 3)) (list? (cons 1 2)) (list? (cons 1 (cons 2 3))) (list? 5))
---
stdout = "(#t #t #t #f #f #f)"
