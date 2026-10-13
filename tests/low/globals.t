(globals 2)

(def main () -> (val) ()
     (global-set! (raw 1) (cons 1 2))
     (cons (global-ref (raw 0)) (global-ref (raw 1))))
---
stdout = "(#<undef> 1 . 2)"
