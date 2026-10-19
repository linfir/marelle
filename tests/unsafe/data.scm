;; Data words start at zero and keep their values across calls
(define-data counter 1)
(define-data table 3)

(define (bump (raw i))
  (declare unsafe noalloc (result raw))
  (store table i (raw+ (load table i) (raw 1)))
  (store counter (raw 0) (raw+ (load counter (raw 0)) (raw 1)))
  (load table i))

(define (entry)
  (declare unsafe)
  (bump (raw 2))
  (bump (raw 0))
  (let ((a (bump (raw 2))))
    (as-val (raw+ (raw<< (raw+ (raw* a (raw 10)) (load counter (raw 0))) (raw 3))
                  (raw 1)))))
(entry)
