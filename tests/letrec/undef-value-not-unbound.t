(define (f)
  (define x (if #f #f))
  (define (g) x)
  (g)
  1)
(f)
---
stdout = "1"
