;; R7RS number procedures, which Chez lacks
(define (exact-integer? x)
  (and (integer? x) (exact? x)))
