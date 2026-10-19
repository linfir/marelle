(define (count n acc)
  (let ((m (- n 1))
        (a (+ acc 2)))
    (if (= n 0)
        acc
        (count m a))))
(count 1000000 0)
