(let ((x 1))
  (let ((f (lambda () (set! x 2))))
    (let ((y x))
      (f)
      (cons y x))))
---
stdout = "(1 . 2)"
