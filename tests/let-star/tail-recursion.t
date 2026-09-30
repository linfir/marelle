(define (loop n) (let* ((m (- n 1))) (if (= m 0) 0 (loop m))))
(loop 1000000)
---
stdout = "0"
