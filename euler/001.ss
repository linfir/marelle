;; Euler problem 001

(define (sum-upto n)
  (/ (* n (+ n 1)) 2))


(define (multiples-upto k n)
  (* k (sum-upto (quotient n k))))


(define (answer n)
  (- (+ (multiples-upto 3 n) (multiples-upto 5 n))
     (multiples-upto 15 n)))


(display (answer (- 1000 1)))
