(define a (f 1))
(define (f x) x)
a
---
stage = "executable"
stderr = "Error: expected procedure: #<undef>\n"
chez = "skip"
