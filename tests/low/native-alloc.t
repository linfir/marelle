(def f ((val x)) -> (val) (:native)
     (cons x x))

(def main () -> (val) ()
     (f 1))
---
stage = "low"
stderr_contains = "safepoint or self in a native def"
