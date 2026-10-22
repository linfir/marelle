(define (loop n sum)
  (if (= n 0)
      sum
      (let ((p (cons n '())))
        (let ((a (cons 0 0)) (b (cons 0 0)) (c (cons 0 0)) (d (cons 0 0)))
          (loop (- n 1) (+ sum (car p) (car a) (car b) (car c) (car d)))))))
(loop 10000000 0)
