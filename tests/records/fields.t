(define-record-type point (fields x y))
(define p (make-point 1 2))
(list (point-x p) (point-y p) (point? p))
---
stdout = "(1 2 #t)"
