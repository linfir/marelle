(define (f n) (if (zero? n) 0 (f (- n 1))))
(f 1000000)
---
stdout = "0"
