(cons (map char-upper-case? (cons #\A (cons #\a '()))) (map char-lower-case? (cons #\A (cons #\a '()))))
---
stdout = "((#t #f) #f #t)"
