(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
(cons (fact 30) (fact 100))
