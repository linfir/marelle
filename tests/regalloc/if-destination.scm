(define (pick x) (if (< x 0) (- 0 x) (* x 2)))
(let ((a 3) (b -4))
  (+ (if (< a b) a b) (+ (pick a) (pick b))))
