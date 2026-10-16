(cons (char>? #\a #\b) (cons (char>? #\b #\a) (cons (char>? #\a #\a) '())))
---
stdout = "(#f #t #f)"
