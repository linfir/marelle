(let ((x 1))
  (let ((f (lambda () (set! x 10) 0)))
    (cons x (f))))
---
stdout = "(1 . 0)"
