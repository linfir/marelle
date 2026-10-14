(def main () -> (val) ()
     (let ((x 1))
       (let* ((y 2)
              (x 3))
         x)))
---
stage = "low"
stderr_contains = "shadowed variable"
