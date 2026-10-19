;; A self tail loop reuses its frame: no collection
(define (count n)
  (if (= n 0)
      0
      (count (- n 1))))
(count 1000000)
