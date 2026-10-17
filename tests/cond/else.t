(define (sign n) (cond ((< n 0) -1) ((= n 0) 0) (else 1)))
(cons (sign -5) (cons (sign 0) (sign 7)))
---
stdout = "(-1 0 . 1)"
