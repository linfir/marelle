;; Chunks of 9 digits with leading zeros: 10^18, 10^27 + 1
(define (pow b n) (if (= n 0) 1 (* b (pow b (- n 1)))))
(cons (pow 10 18) (cons (+ (pow 10 27) 1) (- 0 (pow 10 36))))
---
stdout = "(1000000000000000000 1000000000000000000000000001 . -1000000000000000000000000000000000000)"
