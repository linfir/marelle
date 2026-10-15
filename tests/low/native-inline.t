(def f ((val x)) -> (val) (:native :inline)
     x)

(def main () -> (val) ()
     (f 1))
---
stage = "low"
stderr_contains = ":native with another attribute"
