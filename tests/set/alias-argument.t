(define (f a)
  (let ((b a))
    (set! a 5)
    b))
(f 1)
---
stdout = "1"
