(define (count n acc)
  (if (= n 0)
      acc
      (count (- n 1) (+ acc 1))))
(count 1000000 0)
---
stdout = "1000000"
