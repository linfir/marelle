;; The inlined body uses the caller's names for its own variables
(define (f x y)
  (declare unsafe inline)
  (let ((z (+ x 1)))
    (set! x 100)
    (cons x (cons y (cons z '())))))

(define (entry)
  (declare unsafe)
  (let ((x 1) (z 2) (y 3))
    (cons (f z x) (cons x (cons y (cons z '()))))))
(entry)
---
stdout = "((100 1 3) 1 3 2)"
