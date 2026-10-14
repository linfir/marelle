(def bump ((val x)) -> (val) (:inline)
     (set! x (+ x 10))
     x)

(def f ((val y)) -> (val) ()
     (let ((z y))
       (set! z (+ z 100))
       (cons (bump y) (cons z y))))

(def main () -> (val) ()
     (f 1))
---
stdout = "(11 101 . 1)"
