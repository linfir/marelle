(define (build n acc)
  (if (= n 0)
      acc
      (build (- n 1) (cons n acc))))
(define xs (build 1000 '()))
(define (churn n)
  (if (= n 0)
      0
      (begin (cons 1 2) (churn (- n 1)))))
(define ignored (churn 10000))
(fold-left + 0 xs)
