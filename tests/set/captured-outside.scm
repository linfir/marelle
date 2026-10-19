(let ((x 1))
  (let ((f (lambda () x)))
    (set! x 2)
    (f)))
