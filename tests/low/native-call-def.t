(def g ((val x)) -> (val) ()
     x)

(def f ((val x)) -> (val) (:native)
     (g x))

(def main () -> (val) ()
     (f 1))
---
stage = "low"
stderr_contains = "safepoint or self in a native def"
