(define (f x)
  (declare unsafe noalloc inline)
  x)

(define (entry)
  (declare unsafe)
  (f 1))
(entry)
