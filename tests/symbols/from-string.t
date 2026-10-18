(list (eq? (string->symbol "abc") 'abc)
      (eq? (string->symbol (string-append "ab" "c")) (string->symbol "abc"))
      (string->symbol "xyz"))
---
stdout = '(#t #t xyz)'
