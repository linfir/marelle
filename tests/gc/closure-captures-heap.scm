(define (churn n)
  (if (= n 0)
      0
      (churn (- n 1))))
(define (make-getter l f)
  (lambda (x) (cons (car l) (f x))))
(let ((g (make-getter (cons 1 (cons 2 '())) (lambda (x) (cons x '())))))
  (churn 15000000)
  (g 3))
