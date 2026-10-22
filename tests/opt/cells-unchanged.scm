;; A loop that never assigns a local: the local is no parameter of the loop
(define (f n)
  (let ((base 100))
    (if (< n 0) (set! base 200))
    (let loop ((i 0) (acc 0))
      (if (< i n) (loop (+ i 1) (+ acc base)) acc))))
(list (f 3) (f -1))
