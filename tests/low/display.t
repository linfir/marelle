;; Values the frontend never produces
(def main () -> (val) ()
     (cons (as-val (raw 0))
           (cons (make-box 1)
                 (cons (as-val (raw 4))
                       (cons -5 (as-val (raw 42)))))))
---
stdout = "(#<null> #<unknown ptr> #<err> -5 . #<err>)"
