;; A local assigned in one branch: the join takes its value from each
(define (f x)
  (let ((y 1))
    (if (> x 0) (set! y 2))
    (* y 10)))
(list (f 1) (f -1))
