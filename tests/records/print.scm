(define-record-type point (fields x (mutable y)))
(define-record-type empty)
(list (make-point 1 "a") (make-empty))
