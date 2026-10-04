(define (churn n)
  (if (= n 0)
      0
      (churn (- n 1))))
(define (adders n acc)
  (if (= n 0)
      acc
      (adders (- n 1) (cons (lambda (x) (+ x n)) acc))))
(define (apply-all l acc)
  (if (null? l)
      acc
      (apply-all (cdr l) (+ acc ((car l) 0)))))
(let ((l (adders 100 '())))
  (churn 100000)
  (apply-all l 0))
---
heap = 2000
stdout = "5050"
