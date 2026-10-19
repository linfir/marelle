;; Bignum multiplication and division
(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
(let loop ((i 0) (acc 0))
  (if (= i 100)
      acc
      (loop (+ i 1) (+ acc (remainder (fact 2000) 1000003)))))
