(define (count n) (do ((i 0 (+ i 1))) ((= i n) i)))
(count 1000000)
---
stdout = "1000000"
