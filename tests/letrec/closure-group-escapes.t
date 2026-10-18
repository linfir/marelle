(define (make n)
  (define (get) n)
  (define (twice) (+ (get) (get)))
  twice)
((make 21))
---
stdout = "42"
