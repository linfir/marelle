;; Values the frontend never produces
(define (entry)
  (declare unsafe)
  (cons (as-val (raw 0))
        (cons (make-box 1)
              (cons (as-val (raw 4))
                    (cons -5 (as-val (raw 42)))))))
(entry)
