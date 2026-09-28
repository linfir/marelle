(define (make a b c) (lambda () (cons a (cons b (cons c '())))))
((make 1 2 3))
---
stdout = "(1 2 3)"
