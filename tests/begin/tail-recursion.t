(define (loop n) (begin 0 (if (= n 0) 42 (begin n (loop (- n 1))))))
(loop 1000000)
---
stdout = "42"
