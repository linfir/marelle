;; A tail call to another procedure with 8 arguments
(define (g a b c d e f x y) (cons a y))
(define (f n) (g n 2 3 4 5 6 7 n))
(f 9)
---
stdout = "(9 . 9)"
