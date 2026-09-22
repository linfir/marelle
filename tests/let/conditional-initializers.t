(let ((x (if 1 40 0))
      (y (if 0 0 2)))
  (+ x y))
---
stdout = "40"
