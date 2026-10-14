(def f ((val x)) -> (val) ()
     (let ((y x))
       y))

(def main () -> (val) ()
     (let ((x (let ((y 1)) y))
           (z (let ((y 2)) y)))
       (cons (f x) z)))
---
stdout = "(1 . 2)"
