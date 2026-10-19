;; Carries and borrows across limbs
(define (pow b n) (if (= n 0) 1 (* b (pow b (- n 1)))))
(cons (+ (- (pow 2 192) 1) 1)
      (cons (- (pow 2 192) 1)
            (* (- (pow 2 128) 1) (- (pow 2 128) 1))))
