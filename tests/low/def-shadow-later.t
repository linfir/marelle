(def main () -> (val) ()
     (let ((g 1))
       g))

(def g () -> (val) ()
     2)
---
stage = "low"
stderr_contains = "def named like a local"
