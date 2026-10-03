(define (count n) (and #t (if (= n 0) 42 (count (- n 1)))))
(count 1000000)
---
stdout = "42"
