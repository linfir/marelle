(data f 1 ())

(def f () -> (val) ()
     1)

(def main () -> (val) ()
     (f))
---
stage = "low"
stderr_contains = "name already defined"
