(define e (list (cons 'a 1) (cons 'b 2) (cons 'c 3)))
(list (assq 'b e) (assq 'd e) (assq 'a '()))
---
stdout = '((b . 2) #f #f)'
