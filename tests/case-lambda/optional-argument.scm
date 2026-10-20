(define make-counter
  (case-lambda
   (() (make-counter 0))
   ((start) (lambda () (set! start (+ start 1)) start))))
(let ((c (make-counter)) (d (make-counter 10)))
  (c)
  (list (c) (d)))
