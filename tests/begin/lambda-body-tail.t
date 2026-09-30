(define (f n) ((lambda (m) m (if (= m 0) 0 (f (- m 1)))) n))
(f 1000000)
---
stdout = "0"
