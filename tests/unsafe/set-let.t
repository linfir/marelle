(define (entry)
  (declare unsafe)
  (let ((x 1))
    (let ((y x))
      (set! x (+ x 1))
      (cons x y))))
(entry)
---
stdout = "(2 . 1)"
