;; A non-tail recursive closure: lifted, called directly with its capture, but not contified
(define (f n k)
  (define (sum i)
    (if (= i 0) 0 (+ k (sum (- i 1)))))
  (sum n))
(f 4 10)
