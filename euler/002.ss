;; Euler problem 002

(define (sum-of-even-fibo-upto N)
  (let loop ((i 0)
             (j 1)
             (s 0))
    (if (>= j N)
        s
        (loop j
              (+ i j)
              (if (even? j)
                  (+ s j)
                  s)))))


(display (sum-of-even-fibo-upto 4000000))
