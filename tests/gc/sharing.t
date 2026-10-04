(define (churn n)
  (if (= n 0)
      0
      (churn (- n 1))))
(let* ((p (cons 1 2))
       (l (cons p p)))
  (churn 15000000)
  (eq? (car l) (cdr l)))
---
stdout = "#t"
