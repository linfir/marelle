(globals 1)

(def main () -> (val) ()
     (if (begin (global-set! (raw 0) 5) (= (global-ref (raw 0)) 5))
         (begin (global-set! (raw 0) 6) (cons (global-ref (raw 0)) 7))
         #f))
---
stdout = "(6 . 7)"
