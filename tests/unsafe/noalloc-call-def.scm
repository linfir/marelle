(define (g x)
  (declare unsafe)
  x)

(define (f x)
  (declare unsafe noalloc)
  (g x))

(define (entry)
  (declare unsafe)
  (f 1))
(entry)
