;; Euler problem 003

(include "utils.ss")

(define N 10000)
(define primes (eratosthenes N))


(define (divide-fully n p)
  (if (zero? (modulo n p))
      (divide-fully (/ n p) p)
      n))


(define (biggest-factor n)
  (unless (and (integer? n) (exact? n) (>= n 2))
    (error 'biggest-factor "need an integer ≥ 2" n))
  (let loop ((n n)
             (primes primes))
    (if (null? primes)
        (if (< n (* N N))
            n
            'unknown)
        (let* ((p (car primes))
               (m (divide-fully n p)))
          (if (= m 1)
              p
              (loop m (cdr primes)))))))


(display (biggest-factor 600851475143))
