(def f ((val x)) -> (val) (:inline :extern))

(def main () -> (val) ()
     (f 1))
---
stage = "low"
stderr_contains = "def without a body"
