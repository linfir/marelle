(define (f x) (+ x 1))
(let ((a 5) (b 7))
  (let ((c (f a)))
    (- (* a b) c)))
