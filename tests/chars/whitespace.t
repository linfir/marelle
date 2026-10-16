(map char-whitespace? (cons #\space (cons #\tab (cons #\newline (cons #\return (cons #\page (cons #\a (cons #\nul '()))))))))
---
stdout = "(#t #t #t #t #t #f #f)"
