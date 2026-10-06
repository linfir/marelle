(let ((set! (lambda (a b) (+ a b))))
  (set! 1 2))
---
stdout = "3"
