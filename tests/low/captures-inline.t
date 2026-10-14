(def f () -> (val) (:inline (:captures 1))
     (closure-ref (raw 0)))

(def main () -> (val) ()
     1)
---
stage = "low"
stderr_contains = ":captures requires"
