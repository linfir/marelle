(def f ((val x)) -> (val) (:native)
     x)

(def main () -> (val) ()
     f)
---
stage = "low"
stderr_contains = "native def without a procedure object"
