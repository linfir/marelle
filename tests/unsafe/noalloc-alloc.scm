(define (f x)
  (declare unsafe noalloc)
  (cons x x))

(define (entry)
  (declare unsafe)
  (f 1))
(entry)
