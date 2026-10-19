;; Non-tail calls and fixnum arithmetic
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
(fib 35)
