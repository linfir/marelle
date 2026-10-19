(define (range n acc) (if (= n 0) acc (range (- n 1) (cons n acc))))
(length (range 100000 '()))
