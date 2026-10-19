(define (count n) (or (= n 0) (count (- n 1))))
(count 1000000)
