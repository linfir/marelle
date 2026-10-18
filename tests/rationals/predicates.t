(list (number? 1/2) (rational? 1/2) (real? -1/2) (complex? 1/2) (integer? 1/2) (exact-integer? 1/2)
      (exact? 1/2) (inexact? 1/2) (exact 1/2) (integer? (* 1/2 4)))
---
stdout = "(#t #t #t #t #f #f #t #f 1/2 #t)"
