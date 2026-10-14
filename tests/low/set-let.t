(def main () -> (val) ()
     (let ((x 1))
       (let ((y x))
         (set! x (+ x 1))
         (cons x y))))
---
stdout = "(2 . 1)"
