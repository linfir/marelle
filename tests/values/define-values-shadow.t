(define (f car list)
  (define-values (a b) (values car list))
  (+ a b))
(f 1 2)
---
stdout = "3"
