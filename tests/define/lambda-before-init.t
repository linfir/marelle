(define a (f 1))
(define f (lambda (x) x))
a
---
stage = "executable"
stderr = "Error: expected procedure: #<undef>\n"
chez = "skip"
