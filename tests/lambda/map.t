(define (map f l) (if (null? l) '() (cons (f (car l)) (map f (cdr l)))))
(map (lambda (x) (+ x 1)) (cons 1 (cons 2 (cons 3 '()))))
---
stdout = "(2 3 4)"
