;; Results losing limbs: 2^128 + 5 - 2^128, 2^64 - 1 - (2^64 - 2)
(define (pow b n) (if (= n 0) 1 (* b (pow b (- n 1)))))
(cons (- (+ (pow 2 128) 5) (pow 2 128))
      (- (- (pow 2 64) 1) (- (pow 2 64) 2)))
---
stdout = "(5 . 1)"
