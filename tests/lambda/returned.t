(define (get-square) (lambda (x) (* x x)))
((get-square) 6)
---
stdout = "36"
