(define (count n)
  (if (= n 0)
      0
      (+ 1 (count (- n 1)))))
(define (repeat k acc)
  (if (= k 0)
      acc
      (repeat (- k 1) (count 1000000))))
(repeat 20 0)
---
stdout = "1000000"
