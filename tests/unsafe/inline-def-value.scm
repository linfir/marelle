(define (id x)
  (declare unsafe inline)
  x)

(define (entry)
  (declare unsafe)
  (let ((f id))
    (f 1)))
(entry)
