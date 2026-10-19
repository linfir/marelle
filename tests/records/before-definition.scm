;; A procedure defined before the form uses its accessor
(define (get p) (point-x p))
(define-record-type point (fields x))
(get (make-point 7))
