(define (add1 x)
  (declare unsafe)
  (+ x 1))

(define (entry)
  (declare unsafe)
  (let ((f add1))
    (f 1 2)))
(entry)
