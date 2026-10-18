;; Naive letrec*: a variable read before its init is #<undef> (Chez reports an error)
(define (f) (define a b) (define b 1) a)
(f)
---
stdout = "#<undef>"
chez = "skip"
