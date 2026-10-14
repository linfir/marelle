(def add1 ((val x)) -> (val) ()
     (+ x 1))

(def main () -> (val) ()
     (let ((f add1))
       (f 1 2)))
---
stage = "executable"
stderr = "Error: procedure of arity 1 called with 2 arguments\n"
