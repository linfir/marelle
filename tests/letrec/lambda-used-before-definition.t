;; Lambdas are bound first, unchecked: allowed, since R7RS only says it is an error (Chez reports it)
(define (f)
  (define a (g))
  (define (g) 1)
  a)
(f)
---
stdout = "1"
chez = "skip"
