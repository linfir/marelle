(define (count n)
  (define (loop i acc) (if (= i 0) acc (loop (- i 1) (+ acc 1))))
  (loop n 0))
(count 1000000)
