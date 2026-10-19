(let* ((x 1) (y (begin (set! x 5) x)))
  (+ x y))
