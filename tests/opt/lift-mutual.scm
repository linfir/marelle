;; Mutually recursive closures, each capturing the other and a local: lifted together, then
;; contified
(define (f n k)
  (define (ev? i) (if (= i 0) k (od? (- i 1))))
  (define (od? i) (if (= i 0) (- k) (ev? (- i 1))))
  (ev? n))
(- (f 10 5) (f 7 5))
