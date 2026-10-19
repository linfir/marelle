;; Through apply in tail position, in a small heap
(define (count n) (if (= n 0) 0 (apply count (list (- n 1)))))
(count 100000)
