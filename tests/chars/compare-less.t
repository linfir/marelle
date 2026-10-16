(cons (char<? #\a #\b) (cons (char<? #\b #\a) (cons (char<? #\a #\a) '())))
---
stdout = "(#t #f #f)"
