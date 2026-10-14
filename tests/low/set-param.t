(def inc ((val x)) -> (val) ()
     (set! x (+ x 1))
     x)

(def bump ((val x)) -> (val) (:inline)
     (set! x (+ x 10))
     x)

(def main () -> (val) ()
     (let ((y 1))
       (cons (inc y) (cons (bump y) y))))
---
stdout = "(2 11 . 1)"
