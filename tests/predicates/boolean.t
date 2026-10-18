(list (boolean? #f) (boolean? #t) (boolean? 0) (boolean? 1) (boolean? (quote ())) (boolean? #\a) (boolean? (cons #f #t)))
---
stdout = "(#t #t #f #f #f #f #f)"
