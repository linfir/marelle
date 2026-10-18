(list (equal? "abc" (string-append "ab" "c")) (equal? "abc" "abd") (equal? "" "")
      (equal? "λ" (string #\λ)) (equal? "a" #\a) (equal? (list "x" "y") (list "x" "y")))
---
stdout = '(#t #f #t #t #f #t)'
