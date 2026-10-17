;; Naive letrec: a variable read before its init is #<undef> (Chez reports an error)
(letrec ((a b) (b 1)) a)
---
stdout = "#<undef>"
chez = "skip"
