(define-record-type point (fields x) (protocol (lambda (new) (lambda () (new 0)))))
(point-x (make-point))
