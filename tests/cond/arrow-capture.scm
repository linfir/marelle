(define (adder n) (lambda (x) (+ x n)))
(let ((k 10))
  (cond (k => (adder k))))
