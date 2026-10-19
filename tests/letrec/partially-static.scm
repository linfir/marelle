;; inc captures nothing, so add calls it directly though add captures n
(define (f n)
  (define (inc x) (+ x 1))
  (define (add x) (+ (inc x) n))
  (add 1))
(f 40)
