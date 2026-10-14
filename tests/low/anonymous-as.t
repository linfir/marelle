(def f () -> (val) (:anonymous (:as g))
     1)

(def main () -> (val) ()
     1)
---
stage = "low"
stderr_contains = ":as and :anonymous"
