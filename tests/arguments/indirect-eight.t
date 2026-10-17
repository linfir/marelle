(define (call g) (g 1 2 3 4 5 6 7 8))
(call (lambda (a b c d e f g h) (cons (+ a b) (+ g h))))
---
stdout = "(3 . 15)"
