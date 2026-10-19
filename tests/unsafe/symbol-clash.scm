;; a-b and a_b both give the symbol a_b_code; noalloc functions keep their names
(define (a-b x)
  (declare unsafe noalloc)
  x)

(define (a_b x)
  (declare unsafe noalloc)
  x)

(define (entry)
  (declare unsafe)
  (a-b (a_b 1)))
(entry)
