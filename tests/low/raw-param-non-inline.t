(def f ((raw x)) -> (val) ()
     #t)

(def main () -> (val) ()
     (f (raw 1)))
---
stage = "low"
stderr_contains = "raw parameter or result in a non-inline def"
