(def main () -> (val) ()
     (set! x 1)
     2)
---
stage = "low"
stderr_contains = "unbound variable"
