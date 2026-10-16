(cons (map char->integer (cons #\a (cons #\b '()))) (map integer->char (cons 99 '())))
---
stdout = "((97 98) #\\c)"
