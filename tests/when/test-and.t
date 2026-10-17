(define (f x) (when (and (< 0 x) (< x 10)) x))
(cons (f 5) (f 0))
---
stdout = "(5 . #<undef>)"
chez = "skip"
