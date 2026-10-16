;; Character literals are immediates
(def main () -> (val) ()
     (if (raw= (as-raw #\a) (raw 24866)) ; (97 << 8) | 34
         #\b
         #\c))
---
stdout = "#\\b"
