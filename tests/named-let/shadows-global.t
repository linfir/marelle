(define (loop x) 99)
(let loop ((i 0)) (if (< i 3) (loop (+ i 1)) i))
---
stdout = "3"
