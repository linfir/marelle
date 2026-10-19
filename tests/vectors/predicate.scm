(define (pow b n) (if (= n 0) 1 (* b (pow b (- n 1)))))
(map vector? (cons '#() (cons (cons 1 2) (cons 1 (cons car (cons (pow 2 100) '()))))))
