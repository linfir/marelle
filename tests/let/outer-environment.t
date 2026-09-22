(let ((x 10))
  (let ((x 20) (y x))
    (+ x y)))
---
stdout = "30"
