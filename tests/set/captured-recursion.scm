(let ((f #f))
  (set! f (lambda (n) (if (= n 0) 0 (+ n (f (- n 1))))))
  (f 100))
