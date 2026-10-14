(def main () -> (val) ()
     (let ((x 1))
       (set! x (raw 2))
       x))
---
stage = "low"
stderr_contains = "expected a val"
