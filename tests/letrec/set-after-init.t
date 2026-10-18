(define (f)
  (define a 1)
  (define (set-a!) (set! a 2))
  (set-a!)
  a)
(f)
---
stdout = "2"
