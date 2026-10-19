(define (app a b) (if (null? a) b (cons (car a) (app (cdr a) b))))
(app (cons 1 (cons 2 '())) (cons 3 (cons 4 '())))
