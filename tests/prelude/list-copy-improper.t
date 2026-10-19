;; The improper tail is kept
(list-copy (cons 1 (cons 2 3)))
---
stdout = "(1 2 . 3)"
chez = "skip"
