;; A loop in tail position
(define (entry)
  (declare unsafe)
  (let loop ((i 0) (acc 0))
    (if (= i 10)
        acc
        (loop (+ i 1) (+ acc i)))))
(entry)
