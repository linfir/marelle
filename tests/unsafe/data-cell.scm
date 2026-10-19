;; A static cell through data, with an inline void define
(define-data cell 1)
(define (put! x)
  (declare unsafe inline (result void))
  (store cell (raw 0) x))
(define (get)
  (declare unsafe)
  (as-val (load cell (raw 0))))
(begin (put! 42) (get))
