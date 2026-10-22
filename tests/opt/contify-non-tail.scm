;; A non-tail self call keeps the function; its caller still calls it
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(define (f n) (* 10 (fib n)))
(f 10)
