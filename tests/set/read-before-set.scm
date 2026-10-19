(let ((x 1))
  (+ (begin (set! x 10) x) (begin (set! x 20) x)))
