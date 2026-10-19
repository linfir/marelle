(define (id x) x)
(let loop ((i 0) (acc 0) (k 3))
  (if (= i 1000) (+ acc k) (loop (+ i 1) (+ acc (id k)) k)))
