(list (string=? "" "") (string=? "abc" "abc") (string=? "abc" "abd") (string=? "abc" "ab")
      (string=? "é" "e") (string=? "aé" "aé") (string=? "abcdefghij" "abcdefghik"))
---
stdout = '(#t #t #f #f #f #t #f)'
