;; A recursive def is not inlined into itself; a small caller of it inlines it
(define (count n acc) (if (= n 0) acc (count (- n 1) (+ acc 1))))
(define (start n) (count n 0))
(define (twice n) (+ (start n) (start n)))
(twice 1000)
