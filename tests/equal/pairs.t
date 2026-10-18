(list (equal? '() '()) (equal? (list 1 (list 2 3) 4) (list 1 (list 2 3) 4))
      (equal? (list 1 2) (list 1 2 3)) (equal? (cons 1 2) (cons 1 2)) (equal? (cons 1 2) (cons 1 3))
      (equal? (list 'a) 'a))
---
stdout = '(#t #t #f #t #f #f)'
