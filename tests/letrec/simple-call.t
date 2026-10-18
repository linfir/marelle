(define (f)
  (define a 20)
  (define b (+ a 1))
  (define (g) (* b 2))
  (g))
(f)
---
stdout = "42"
