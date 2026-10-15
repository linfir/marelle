(def main () -> (val) ()
     (let loop ((i 0))
       (let ((loop 1))
         loop)))
---
stage = "low"
stderr_contains = "shadowed variable"
