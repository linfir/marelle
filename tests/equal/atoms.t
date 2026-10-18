(list (equal? 1 1) (equal? 1 2) (equal? #\a #\a) (equal? #t #f) (equal? 'a 'a) (equal? 'a 'b)
      (equal? car car) (equal? 1 #\a))
---
stdout = '(#t #f #t #f #t #f #t #f)'
