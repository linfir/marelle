(define (f (raw x))
  (declare unsafe)
  #t)

(define (entry)
  (declare unsafe)
  (f (raw 1)))
(entry)
