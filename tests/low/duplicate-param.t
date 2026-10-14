(def f ((val x) (val x)) -> (val) ()
     x)

(def main () -> (val) ()
     (f 1 2))
---
stage = "low"
stderr_contains = "shadowed variable"
