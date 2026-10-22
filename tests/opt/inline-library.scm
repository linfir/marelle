;; A small library procedure is inlined into a program
(define (f x) (square x))
(list (f 5) (f 7))
