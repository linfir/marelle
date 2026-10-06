(let ((x 1))
  (let ((g (lambda () x)))
    (let ((x 5))
      (set! x 6)
      (+ x (g)))))
---
stdout = "7"
