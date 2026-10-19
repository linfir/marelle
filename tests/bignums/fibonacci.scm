(define (fib n a b) (if (= n 0) a (fib (- n 1) b (+ a b))))
(fib 300 0 1)
