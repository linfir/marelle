;; A literal is a new vector at each evaluation
(define (f) '#(0))
(let ((v (f)))
  (vector-set! v 0 1)
  (cons v (f)))
---
stdout = "(#(1) . #(0))"
chez = "skip"
