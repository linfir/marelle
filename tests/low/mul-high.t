;; The high half of unsigned products: (2^64 - 1)^2 = (2^64 - 2) * 2^64 + 1
(def main () -> (val) ()
     (cons (raw= (raw*hu (raw -1) (raw -1)) (raw -2))
           (cons (as-val (raw+ (raw<< (raw*hu (raw 1000) (raw 1000)) (raw 3)) (raw 1)))
                 (as-val (raw+ (raw<< (raw*hu (raw 4611686018427387904) (raw 8)) (raw 3))
                               (raw 1))))))
---
stdout = "(#t 0 . 2)"
