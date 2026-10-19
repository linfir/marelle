(define-record-type point (fields x y))
(define-record-type other (fields x y))
(point-y (make-other 1 2))
