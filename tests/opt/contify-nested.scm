;; A contified loop calling another one, both into the same caller
(define (f n)
  (define (inner i acc) (if (= i 0) acc (inner (- i 1) (+ acc 1))))
  (define (outer j m acc) (if (= j 0) acc (outer (- j 1) m (inner m acc))))
  (outer n n 0))
(f 7)
