(define (f x y)
  (if (< x y) (f (+ x 1) y) (list x y)))
(f 0 3)
