(let ((x 1))
  (let ((f (lambda () (set! x 2))))
    (f)
    x))
