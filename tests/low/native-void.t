(def show ((raw n)) -> () (:native)
     (put-int (stdout) n)
     (put-string (stdout) " "))

(def main () -> (val) ()
     (show (raw 7))
     0)
---
stdout = "7 0"
