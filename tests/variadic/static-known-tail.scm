;; Known calls pack the extra arguments, tail calls too
(define (count n . r)
  (declare public)
  (if (= n 0) r (count (- n 1) n)))
(count 1000000)
