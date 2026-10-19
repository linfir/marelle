(let ((x 1))
  (set! x 2)
  (let ((f (let ((x 5)) (lambda () x))))
    (cons x (f))))
