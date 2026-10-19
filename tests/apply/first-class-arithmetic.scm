(define (iota n acc) (if (= n 0) acc (iota (- n 1) (cons n acc))))
(cons (apply * (iota 10 (list))) (cons (apply < (iota 5 (list))) (apply - (list 10 1 2))))
