(define (f n) (define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact n))
(f 10)
---
stdout = "3628800"
