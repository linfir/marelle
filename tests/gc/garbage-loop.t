(define (loop n acc)
  (if (= n 0)
      acc
      (loop (- n 1) (cons n '()))))
(loop 10000000 '())
---
stdout = "(1)"
