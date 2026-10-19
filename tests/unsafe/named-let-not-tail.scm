;; Not a loop: a procedure, so its variables are vals
(define (entry)
  (declare unsafe)
  (let loop ((i 0))
    (if (raw= (as-raw i) (as-raw 5)) 0 (+ 1 (loop (+ i 1))))))
(entry)
