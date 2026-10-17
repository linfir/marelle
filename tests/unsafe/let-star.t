(define (entry)
  (declare unsafe)
  (let* ((x 1)
         (y (+ x 1))
         (z (+ y 10)))
    (cons z y)))
(entry)
---
stdout = "(12 . 2)"
