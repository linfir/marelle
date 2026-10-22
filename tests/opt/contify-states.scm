;; A state machine of three procedures calling each other in tail position: contified
(define (g n)
  (define (s1 i acc) (if (= i 0) acc (s2 (- i 1) (+ acc 1))))
  (define (s2 i acc) (if (= i 0) acc (s3 (- i 1) (+ acc 2))))
  (define (s3 i acc) (if (= i 0) acc (if (odd? acc) (s1 (- i 1) acc) (s2 (- i 1) (+ acc 3)))))
  (s1 n 0))
(g 10)
