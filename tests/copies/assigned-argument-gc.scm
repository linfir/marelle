;; b reads the argument a after collections moved its list
(define sink #f)
(define (churn n)
  (if (= n 0)
      0
      (begin (set! sink (cons n n)) (churn (- n 1)))))
(define (f a)
  (set! a (list a 2))
  (let ((b a))
    (churn 10000)
    (set! a 0)
    (list a (car b) (car (cdr b)))))
(f 7)
