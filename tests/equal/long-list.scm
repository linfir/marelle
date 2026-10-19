(define (iota n acc) (if (= n 0) acc (iota (- n 1) (cons n acc))))
(equal? (iota 1000000 '()) (iota 1000000 '()))
