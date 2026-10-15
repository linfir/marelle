(data counter 1 ())

(def main () -> (val) ()
     (let ((counter 1))
       counter))
---
stage = "low"
stderr_contains = "local variable named like a def"
