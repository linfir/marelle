;; A tag holds across a call, a header does not: the call may write it
(define (f v g)
  (let ((a (vector-ref v 0)))
    (g)
    (+ a (vector-ref v 1))))
(f (vector 1 2) (lambda () 0))
