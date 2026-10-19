(define (f x)
  (declare unsafe noalloc)
  x)

(define (entry)
  (declare unsafe)
  f)
(entry)
