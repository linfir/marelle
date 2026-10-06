(let* ((x 1)
       (f (lambda () (set! x (* x 10))))
       (y (begin (f) x)))
  (f)
  (cons x y))
---
stdout = "(100 . 10)"
