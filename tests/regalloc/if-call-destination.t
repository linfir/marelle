(define (f x) (+ x 1))
(define (g x) (* x 2))
(define (h c x)
  (let ((y (if c (f x) (g x))))
    (+ y (f y))))
(+ (h #t 3) (h #f 3))
---
stdout = "22"
