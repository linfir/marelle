;; Logical shift right: -1 >>u 60 is 15, while -1 >> 60 stays -1
(def main () -> (val) ()
     (cons (as-val (raw+ (raw<< (raw>>u (raw -1) (raw 60)) (raw 3)) (raw 1)))
           (as-val (raw+ (raw<< (raw>> (raw -1) (raw 60)) (raw 3)) (raw 1)))))
---
stdout = "(15 . -1)"
