(def main () -> (val) ()
     (let* ((x 1)
            (y (+ x 1))
            (z (+ y 10)))
       (cons z y)))
---
stdout = "(12 . 2)"
