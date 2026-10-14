(def loop ((val n)) -> (val) (:inline)
     (loop n))

(def main () -> (val) ()
     (loop 1))
---
stage = "low"
stderr_contains = "recursive inline def"
