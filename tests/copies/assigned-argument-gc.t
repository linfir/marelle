;; b reads the argument a after collections moved its list
(define (churn n)
  (if (= n 0)
      0
      (begin (cons n n) (churn (- n 1)))))
(define (f a)
  (set! a (list a 2))
  (let ((b a))
    (churn 10000)
    (set! a 0)
    (list a (car b) (car (cdr b)))))
(f 7)
---
stdout = "(0 7 2)"
env = { SCHEME_HEAP = "2500" }
