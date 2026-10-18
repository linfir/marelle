;; R7RS number procedures, which Chez lacks
(define (exact-integer? x)
  (and (integer? x) (exact? x)))

;; Ours, rounding ties up
(define (round-half-up x)
  (floor (+ x 1/2)))

(define (square x)
  (* x x))

(define (floor-quotient n m)
  (floor (/ n m)))

(define (floor-remainder n m)
  (modulo n m))

(define (floor/ n m)
  (values (floor-quotient n m) (floor-remainder n m)))

(define (truncate-quotient n m)
  (quotient n m))

(define (truncate-remainder n m)
  (remainder n m))

(define (truncate/ n m)
  (values (quotient n m) (remainder n m)))
