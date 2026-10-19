(define (count n)
  (if (= n 0)
      0
      (+ 1 (count (- n 1)))))
(count 2000000)
