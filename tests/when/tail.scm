(define (count n) (unless (= n 0) (count (- n 1))))
(when (pair? (cons 1 2)) (count 1000000) 7)
