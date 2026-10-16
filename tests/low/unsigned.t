;; Unsigned comparisons: (raw -1) is the largest raw
(def bit ((val test) (raw n)) -> (raw) (:inline)
     (if test (raw<< (raw 1) n) (raw 0)))

(def main () -> (val) ()
     (let ((m (raw -1))
           (one (raw 1)))
       (as-val (raw+ (raw<< (raw+ (bit (raw>u m one) (raw 0))
                                  (raw+ (bit (raw>=u m m) (raw 1))
                                        (raw+ (bit (raw<u m one) (raw 2))
                                              (raw+ (bit (raw<=u one m) (raw 3))
                                                    (bit (raw> m one) (raw 4))))))
                         (raw 3))
                     (raw 1)))))
---
stdout = "11"
