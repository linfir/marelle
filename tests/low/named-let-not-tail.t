(def main () -> (val) ()
     (let loop ((i 0))
       (+ 1 (loop i))))
---
stage = "low"
stderr_contains = "called not in exit position"
