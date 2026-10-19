(define (range n acc) (if (= n 0) acc (range (- n 1) (cons n acc))))
(define (len l acc) (if (null? l) acc (len (cdr l) (+ acc 1))))
(len (range 100000 '()) 0)
