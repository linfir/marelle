;; R7RS number procedures, which Chez lacks
(define (exact-integer? x)
  (and (integer? x) (exact? x)))

;; Ours, rounding ties up
(define (round-half-up x)
  (floor (+ x 1/2)))
