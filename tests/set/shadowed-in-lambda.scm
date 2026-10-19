(let ((x 1))
  (let ((f (lambda (x) (set! x 2) x)))
    (+ x (f 0))))
