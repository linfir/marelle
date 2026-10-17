;; A variadic procedure calling itself through its global, each time with a fresh list
(define (count n . r)
  (if (= n 0) r (count (- n 1) n n)))
(count 100000)
---
stdout = "(1 1)"
