(let ((f car))
  (set! f cdr)
  (f (cons 1 2)))
