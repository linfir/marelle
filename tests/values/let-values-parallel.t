(let ((a 1) (b 2))
  (let-values (((a b) (values b a)) ((c . d) (values a b 9)))
    (list a b c d)))
---
stdout = "(2 1 1 (2 9))"
