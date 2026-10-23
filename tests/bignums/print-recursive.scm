;; display splits big numbers by powers 10^(18 * 2^k), dividing recursively; number->string
;; does not, so each must agree with it
(define (displayed x)
  (let ((p (open-output-string)))
    (display x p)
    (get-output-string p)))
(define (agrees? x)
  (and (string=? (displayed x) (number->string x))
       (string=? (displayed (- x)) (number->string (- x)))))
;; Of the form z 2^(64j) 10^(18h) - 1, some divisions see their dividend's top limbs equal to
;; the divisor's
(define (failures h j)
  (if (>= j h)
      '()
      (let ((x (- (* (expt 2 (* 64 j)) (expt 10 (* 18 h))) 1))
            (rest (failures h (+ j (quotient h 4)))))
        (if (and (agrees? x) (agrees? (* 3 x)) (agrees? (+ x 2)))
            rest
            (cons (list h j) rest)))))
(list (failures 64 1) (failures 128 1)
      (agrees? (- (expt 10 2304) 1)) (agrees? (expt 10 2304)) (agrees? (expt 3 20000))
      (agrees? (* (expt 10 1152) (+ (expt 2 640) 12345))))
