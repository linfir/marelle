(define (square x) (* x x))
(define (map f l) (if (null? l) '() (cons (f (car l)) (map f (cdr l)))))
(map square (cons 1 (cons 2 (cons 3 '()))))
