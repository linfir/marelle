(define (entry)
  (declare unsafe)
  (let ((x 1))
    (cons x (begin (set! x 2) x))))
(entry)
---
stdout = "(1 . 2)"
