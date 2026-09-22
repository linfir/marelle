(let ((x 10) (y 20))
  (let ((x y) (y x))
    (- x y)))
---
stdout = "10"
