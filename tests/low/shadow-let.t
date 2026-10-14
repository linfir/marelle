(def main () -> (val) ()
     (let ((x 1)
           (x 2))
       x))
---
stage = "low"
stderr_contains = "shadowed variable"
