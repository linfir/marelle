(define (f x)
  (declare unsafe)
  (let ((y x))
    y))

(define (entry)
  (declare unsafe)
  (let ((x (let ((y 1)) y))
        (z (let ((y 2)) y)))
    (cons (f x) z)))
(entry)
---
stdout = "(1 . 2)"
