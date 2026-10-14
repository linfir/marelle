(def id ((val x)) -> (val) (:inline)
     x)

(def main () -> (val) ()
     (let ((f id))
       (f 1)))
---
stage = "low"
stderr_contains = "inline def without a procedure object"
