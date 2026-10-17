;; z copies y, which copies x: z keeps the first value
(let ((x 1))
  (set! x 2)
  (let ((y x))
    (let ((z y))
      (set! y 5)
      (set! x 9)
      (list x y z))))
---
stdout = "(9 5 2)"
