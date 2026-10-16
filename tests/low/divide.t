;; raw/ and raw% truncate toward zero
(def int-value ((raw n)) -> (val) (:inline)
     (as-val (raw+ (raw<< n (raw 3)) (raw 1))))

(def main () -> (val) ()
     (cons (int-value (raw/ (raw -7) (raw 2)))
           (cons (int-value (raw% (raw -7) (raw 2)))
                 (cons (int-value (raw/ (raw 7) (raw -2)))
                       '()))))
---
stdout = "(-3 -1 -3)"
