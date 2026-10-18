;; A set! before the init is not checked, since R7RS only says it is an error (Chez reports it)
(define (f)
  (define (set-x!) (set! x 5))
  (define y (set-x!))
  (define x (+ x 1))
  x)
(f)
---
stdout = "6"
chez = "skip"
