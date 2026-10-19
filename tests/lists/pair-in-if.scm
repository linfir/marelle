(define (first-or-self x) (if (pair? x) (car x) x))
(cons (first-or-self (cons 1 2)) (first-or-self 3))
