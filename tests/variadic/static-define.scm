(define (f a . r)
  (declare public)
  (cons a r))
(cons (f 1) (f 1 2 3))
