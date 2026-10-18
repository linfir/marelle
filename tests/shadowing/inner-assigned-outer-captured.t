(define (f x)
  (let ((g (lambda () x)))
    (let ((x 5))
      (set! x 6)
      (+ x (g)))))
(f 1)
---
stdout = "7"
