(def main () -> (val) ()
     (let ((x 1))
       (cons x (begin (set! x 2) x))))
---
stdout = "(1 . 2)"
