(def show ((raw n)) -> () (:native)
     (c-call printf "%ld " n))

(def main () -> (val) ()
     (show (raw 7))
     0)
---
stdout = "7 0"
