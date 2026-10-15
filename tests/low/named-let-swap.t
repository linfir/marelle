;; The arguments are all computed before the variables are set
(def main () -> (val) ()
     (let loop ((a 1) (b 2) (n 3))
       (if (= n 0)
           (cons a b)
           (loop b a (- n 1)))))
---
stdout = "(2 . 1)"
