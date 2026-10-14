(def f ((val car)) -> (val) ()
     car)

(def main () -> (val) ()
     (f 1))
---
stage = "low"
stderr_contains = "local variable named like a def"
