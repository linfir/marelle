(define count
  (case-lambda
   ((n) (count n 0))
   ((n acc) (if (= n 0) acc (count (- n 1) (+ acc 1))))))
(count 1000000)
