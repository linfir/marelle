(define (make-is c) (lambda (x) (char=? x c)))
((make-is #\q) #\q)
---
stdout = "#t"
