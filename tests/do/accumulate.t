(do ((i 0 (+ i 1)) (acc (quote ()) (cons i acc))) ((= i 5) acc))
---
stdout = "(4 3 2 1 0)"
