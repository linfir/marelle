(def main () -> (val) ()
     (let* ((x 1)
            (y (+ x 1))
            (x (+ y 10)))
       (cons x y)))
---
stdout = "(12 . 2)"
