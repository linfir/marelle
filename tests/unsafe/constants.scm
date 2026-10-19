(define-constant base 5)
(define-constant shifted (raw (ash base 3)))
(define-constant flag #t)

(define (entry)
  (declare unsafe)
  (if (and flag (raw= shifted (raw 40))) (+ base 1) 0))
(entry)
