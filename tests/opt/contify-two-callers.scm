;; Called from two functions: stays a function
(define (count l n) (if (null? l) n (count (cdr l) (+ n 1))))
(define (f l) (+ 1 (count l 0)))
(define (g l) (+ 2 (count l 0)))
(list (f '(a b)) (g '(a b c)))
